;;; Copyright 2020 Beckman Coulter, Inc.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

#!chezscheme
(library (tower)
  (export
   tower:running?
   tower:start-server
   tower:sup-spec
   )
  (import
   (chezscheme)
   (json)
   (keywords)
   (software-info)
   (swish imports)
   )

  (define verbosity (make-parameter 0))

  ;; When ranking completions, a reference N lines below the cursor
  ;; counts as N * completion-below-cost lines away.
  (define completion-below-cost 4)

  (define (mapn f ls)
    (let lp ([i 0] [ls ls])
      (if (null? ls)
          '()
          (cons (f i (car ls))
            (lp (+ i 1) (cdr ls))))))

  ;; A request without an id is a notification and gets no response.
  (define (rpc:respond ws req result)
    (let ([id (json:ref req 'id #f)])
      (when id
        (let ([res (json:make-object [jsonrpc "2.0"] [id id])])
          (when result
            (json:extend-object res [result result]))
          (ws:send ws (json:object->string res))))))

  (define (scalar x)
    (match x
      [(#(,v)) v]
      [() #f]))

  (define ($update-keywords timestamp keywords)
    (db:log 'log-db "delete from keywords")
    (for-each
     (lambda (kw)
       (db:log 'log-db "insert into keywords(timestamp,keyword,meta) values(?,?,?)"
         (coerce timestamp)
         (coerce (json:get kw 'keyword))
         (coerce (json:get kw 'meta))))
     keywords))

  (define (maybe-rows rows)
    (and (not (null? rows)) rows))

  (define ($get-unique-id line char filename)
    (scalar
     (execute "
select D.uid
from refs D
inner join files F on F.file_pk=D.file_fk
where D.line=?1
  and ?2 between D.char and D.char + D.len
  and F.filename=?3"
       line char filename)))

  (define ($defns-in-file uid filename)
    (maybe-rows
     (execute "
select F.filename,D.line,D.char,D.len from refs D
inner join files F on F.file_pk=D.file_fk
where D.uid=?1
  and F.filename=?2
  and D.type='defn'
order by D.line asc"
       uid filename)))

  (define ($defns-in-workspace uid root-fk)
    (maybe-rows
     (execute "
select F.filename,D.line,D.char,D.len from refs D
inner join files F on F.file_pk=D.file_fk
where D.uid=?1
  and D.root_fk=?2
  and D.type='defn'
order by substr(F.filename,-3)='.ss' desc, F.filename asc, D.line asc"
       uid root-fk)))

  (define ($refs-in-file line char filename)
    (execute "
select D.line,D.char,D.len from refs D
inner join files F on F.file_pk=D.file_fk
where D.uid in
  (select D.uid
   from refs D
   inner join files F on F.file_pk=D.file_fk
   where D.line=?1
     and ?2 between D.char and D.char + D.len
     and F.filename=?3)
  and F.filename=?3
order by D.line asc, D.char asc"
      line char filename))

  (define ($refs-in-workspace line char filename root-fk)
    (execute "
select F.filename,D.line,D.char,D.len from refs D
inner join files F on F.file_pk=D.file_fk
where D.uid in
  (select D.uid
   from refs D
   inner join files F on F.file_pk=D.file_fk
   where D.line=?1
     and ?2 between D.char and D.char + D.len
     and F.filename=?3)
  and D.root_fk=?4
order by substr(F.filename,-3)='.ss' desc, F.filename asc, D.line asc"
      line char filename root-fk))

  (define ($defns-anywhere uid filename)
    (maybe-rows
     (execute "
select F.filename,D.line,D.char,D.len from refs D
inner join roots R on D.root_fk=R.root_pk
inner join files F on F.file_pk=D.file_fk
where D.uid=?1
  and D.type='defn'
order by F.filename=?2 desc, R.timestamp desc,
substr(F.filename,-3)='.ss' desc, F.filename asc, D.line asc"
       uid filename)))

  (define do-log
    (case-lambda
     [(level message)
      (do-log level (erlang:now) message)]
     [(level timestamp message)
      (db:log 'log-db
        "insert into events(timestamp,pid,message) values(?,?,?)"
        (coerce timestamp)
        (coerce (or (client-pid) self))
        (coerce message))
      (cond
       [(< (verbosity) level) (void)]
       [(json:object? message)
        (json:write (current-output-port) message 0)]
       [else
        (display message)
        (newline)])]))

  ;; The sample standard deviation of count values, from their average
  ;; and the average of their squares. SQLite here has no sqrt. Rounding
  ;; can make the variance slightly negative, so it's at least 0.
  (define (stddev count avg avg2)
    (if (> count 1)
        (sqrt (max 0 (* (- avg2 (* avg avg)) (/ count (- count 1)))))
        0))

  (define (prefix-integer s)
    (if (string=? s "")
        0
        (char->integer (char-downcase (string-ref s 0)))))

  (define (handle-message ws msg)
    (match (string->symbol (json:get msg 'method))
      [log
       (do-log 2
         (or (json:ref msg '(params timestamp) #f) (erlang:now))
         (json:get msg '(params message)))]
      [get-completions
       (let* ([filename (json:get msg '(params filename))]
              [line (json:get msg '(params line))]
              [char (json:get msg '(params char))]
              [prefix (json:get msg '(params prefix))]
              ;; when the identifier contains with a percent or
              ;; underscore, we need to escape it using an uncommon
              ;; character, like #\esc
              [pat (string-append (pregexp-replace* "([_%])" prefix "\x1B;\\1") "%")]
              [root-fk (root-key)]
              [rows
               (transaction 'log-db
                 (let ([file-fk (scalar (execute "select file_pk from files where filename=?" filename))]
                       [pre1 (prefix-integer prefix)])
                   ;; distance: the weighted distance in lines from the
                   ;; cursor to the nearest reference in this file. A
                   ;; new local is most likely defined above the
                   ;; cursor, so references below cost more. Names
                   ;; with no reference in this file have a null
                   ;; distance and sort after the rest.
                   (execute "
select
  min(case when refs.file_fk=?4 then
        case when refs.line <= ?5 then ?5 - refs.line
             else (refs.line - ?5) * ?6 end
      end) as distance
  ,count(refs.name) as count
  ,candidates.name
from
(select keyword as name from keywords where keyword like ?2 escape '\x1B;'
 union
 select name from refs
 where pre1=?1 and name like ?2 escape '\x1B;' and root_fk=?3
) candidates
left outer join refs
on candidates.name = refs.name
group by candidates.name
order by distance is null, distance asc, count desc, candidates.name asc"
                     pre1 pat root-fk file-fk line completion-below-cost)))]
              ;; Drop the identifier under the cursor when it's the
              ;; only reference. It need not be the first row, since a
              ;; candidate on the same line may rank ahead of it.
              [rows
               (remp
                (lambda (row)
                  (match row
                    [#(,_ 1 ,@prefix) #t]
                    [,_ #f]))
                rows)]
              [nrows (length rows)]
              [completions
               (mapn
                (lambda (i row)
                  (match row
                    [#(,distance ,count ,name)
                     (let ([sortText (format "~6d" i)]
                           [score (- 1.0 (/ i nrows))])
                       (json:make-object
                        [label name]
                        ;;[detail (format "| distance:~a | count:~a | sort: ~a | score: ~a" distance count sortText score)]
                        [sortText sortText]
                        [score score]))]))
                rows)])
         (rpc:respond ws msg completions))]
      [get-definitions
       (let* ([filename (json:get msg '(params filename))]
              [line (json:get msg '(params line))]
              [char (json:get msg '(params char))]
              [root-fk (root-key)]
              [defns
               (map
                (lambda (row)
                  (match row
                    [#(,fn ,line ,char ,len)
                     (json:make-object
                      [filename fn]
                      [line line]
                      [char char]
                      [len len])]))
                (transaction 'log-db
                  (let ([uid ($get-unique-id line char filename)])
                    (or ($defns-in-file uid filename)
                        ($defns-in-workspace uid root-fk)
                        ($defns-anywhere uid filename)
                        '()))))])
         (rpc:respond ws msg defns))]
      [get-local-references
       (let* ([filename (json:get msg '(params filename))]
              [line (json:get msg '(params line))]
              [char (json:get msg '(params char))]
              [refs
               (map
                (lambda (row)
                  (match row
                    [#(,line ,char ,len)
                     (json:make-object
                      [line line]
                      [char char]
                      [len len])]))
                (transaction 'log-db
                  ($refs-in-file line char filename)))])
         (rpc:respond ws msg refs))]
      [get-references
       (let* ([filename (json:get msg '(params filename))]
              [line (json:get msg '(params line))]
              [char (json:get msg '(params char))]
              [root-fk (root-key)]
              [refs
               (map
                (lambda (row)
                  (match row
                    [#(,fn ,line ,char ,len)
                     (json:make-object
                      [filename fn]
                      [line line]
                      [char char]
                      [len len])]))
                (transaction 'log-db
                  ($refs-in-workspace line char filename root-fk)))])
         (rpc:respond ws msg refs))]
      [reset-directory
       (let* ([dir (json:get msg '(params directory))]
              [pk
               (transaction 'log-db
                 (match (scalar (execute "select root_pk from roots where path=?" dir))
                   [#f
                    (execute "insert into roots(timestamp,path,meta) values(?,?,?)"
                      (coerce (erlang:now))
                      (coerce dir)
                      (coerce (json:make-object)))
                    (scalar (execute "select last_insert_rowid()"))]
                   [,pk
                    (execute "update roots set timestamp=?, meta=json_patch(meta,?) where path=?"
                      (coerce (erlang:now))
                      (coerce (json:make-object))
                      dir)
                    (execute "delete from refs where root_fk=? or root_fk is null" pk)
                    pk]))])
         (root-key pk)
         (rpc:respond ws msg "ok"))]
      [update-keywords
       (let ([keywords (json:get msg '(params keywords))]
             [start (erlang:now)])
         ($update-keywords start keywords)
         (unless (< (verbosity) 1)
           (transaction 'log-db
             (do-log 1
               (json:make-object
                [_op_ "update-keywords"]
                [keywords (scalar (execute "select count(*) from keywords"))]
                [time (- (erlang:now) start)]))))
         (rpc:respond ws msg "ok"))]
      [update-references
       (let ([filename (json:get msg '(params filename))]
             [refs (json:get msg '(params references))]
             [root-fk (root-key)]
             [start (erlang:now)])
         (assert (path-absolute? filename))
         (db:log 'log-db "insert into files (timestamp,filename) values (?,?) on conflict(filename) do update set timestamp=excluded.timestamp"
           (coerce start)
           (coerce filename))
         (let ([file-fk (transaction 'log-db
                          (scalar (execute "select file_pk from files where filename=?" filename)))])
           (db:log 'log-db "delete from refs where file_fk=?" file-fk)
           (for-each
            (lambda (ref)
              (let* ([meta (json:get ref 'meta)]
                     [name (coerce (json:get ref 'name))]
                     [len (json:get ref 'len)]
                     [uid (json:ref ref 'uid #f)]
                     [uid (or (and uid (coerce uid)) name)])
                (db:log 'log-db "insert into refs(timestamp,root_fk,file_fk,pre1,name,len,uid,type,line,char,meta) values(?,?,?,?,?,?,?,?,?,?,?)"
                  (coerce start)
                  (coerce root-fk)
                  (coerce file-fk)
                  (coerce (prefix-integer name))
                  name
                  len
                  uid
                  (coerce (and (= (json:ref meta 'definition 0) 1)
                               "defn"))
                  (coerce (json:get ref 'line))
                  (coerce (json:get ref 'char))
                  (coerce meta))))
            refs))
         (rpc:respond ws msg "ok"))]
      [log-requests
       (let ([root-fk (root-key)])
         (for-each
          (lambda (r)
            (db:log 'log-db "insert into requests(timestamp,root_fk,method,filename,total) values(?,?,?,?,?)"
              (json:get r 'start)
              (coerce root-fk)
              (json:get r 'method)
              (coerce (json:ref r 'filename #f))
              (json:get r 'total)))
          (json:get msg '(params requests))))]
      [shutdown
       (do-log 1 (json:make-object [_op_ "shutdown"]))
       (rpc:respond ws msg "ok")
       (app:shutdown)]
      ))

  ;; The tower handles a client's message with the client's root and
  ;; logs events with the client's pid.
  (define root-key (make-process-parameter #f))
  (define client-pid (make-process-parameter #f))

  ;; Each connection reads its messages, and the tower gen-server
  ;; handles them one at a time, so a handler's db:log and transaction
  ;; calls aren't interleaved with another connection's.
  (define (client)
    (tower:register)
    (let lp ([root-fk #f])
      (receive
       [#(ws:message ,ws ,msg)
        (match (tower:handle-message ws
                 (if (string? msg)
                     (json:string->object msg)
                     (json:bytevector->object msg))
                 root-fk)
          [`(catch ,reason ,e)
           (do-log 1 (exit-reason->english reason))
           (throw e)]
          [#(ok ,root-fk) (lp root-fk)])]
       [#(ws:closed ,ws ,code ,reason) 'ok]
       [#(ws:init ,ws) (lp root-fk)])))

  (define (tower:start&link)
    (define-state-tuple <tower> waketime clients)

    (define (reply x state)
      `#(reply ,x ,state ,($state waketime)))
    (define (no-reply state)
      `#(no-reply ,state ,($state waketime)))

    (define (init) `#(ok ,(<tower> make [waketime 'infinity] [clients '()])))
    (define (terminate reason state) 'ok)
    (define (handle-call msg from state)
      (match msg
        [#(handle-message ,pid ,ws ,msg ,root-fk)
         ;; A failure goes back to the client, which logs it and exits.
         (reply
          (try
           (parameterize ([root-key root-fk] [client-pid pid])
             (handle-message ws msg)
             `#(ok ,(root-key))))
          state)]
        [#(register ,pid)
         (monitor pid)
         (let ([clients (cons pid ($state clients))])
           (do-log 1
             (json:make-object
              [_op_ "connected"]
              [pid (coerce pid)]
              [total-clients (length clients)]))
           (reply 'ok ($state copy [waketime 'infinity] [clients clients])))]
        [num-clients
         (reply (length ($state clients)) state)]
        [shutdown-time
         (let ([waketime ($state waketime)])
           (reply (and (fixnum? waketime) waketime) state))]))
    (define (handle-cast msg state) (match msg))
    (define (handle-info msg state)
      (match msg
        [`(DOWN ,_ ,pid ,_)
         (let ([clients (remq pid ($state clients))])
           (do-log 1
             (json:make-object
              [_op_ "disconnected"]
              [pid (coerce pid)]
              [total-clients (length clients)]))
           (no-reply ($state copy
                       [clients clients]
                       [waketime
                        (if (null? clients)
                            (+ (erlang:now) 30000)
                            'infinity)])))]
        [timeout
         (app:shutdown)
         `#(no-reply ,state)]))
    (gen-server:start&link 'tower))

  ;; Calls wait for the messages ahead of them, which can take a while.
  (define (tower:handle-message ws msg root-fk)
    (gen-server:call 'tower `#(handle-message ,self ,ws ,msg ,root-fk) 'infinity))

  (define (tower:register)
    (gen-server:call 'tower `#(register ,self) 'infinity))

  (define (tower:num-clients)
    (gen-server:call 'tower 'num-clients 'infinity))

  (define (tower:shutdown-time)
    (gen-server:call 'tower 'shutdown-time 'infinity))

  (define (tower:running? port-number)
    (match (try
            (let-values ([(ip op) (connect-tcp "localhost" port-number)])
              (close-port op)))
      [`(catch ,reason) #f]
      [,_ #t]))

  (define (tower-db:setup)
    (define schema-name 'swish-lint)
    (define schema-version "2020-05-22")
    (define (init-db)
      (define (create-index name sql)
        (execute (format "create index if not exists ~a on ~a" name sql)))

      (create-table events
        [timestamp integer]
        [pid text]
        [message text])
      (create-table files
        [file_pk integer primary key]
        [timestamp integer]
        [filename text unique])
      (create-table keywords
        [timestamp integer]
        [keyword text]
        [meta text])
      (create-table refs
        [timestamp integer]
        [root_fk integer]
        [file_fk integer]
        [pre1 integer]
        [name text]
        [len integer]                   ; precomputed string-length
        [uid text] ; TODO consider using an integer for speed instead
                   ; (need to figure out how to compute a useful value
                   ; when inserting references.)
        [type text]
        [line integer]
        [char integer]
        [meta text])
      (create-table roots
        [root_pk integer primary key]
        [timestamp integer]
        [path text]
        [meta text])
      ;; One row per LSP request, timed by the client: total is the ms
      ;; its worker took.
      (create-table requests
        [timestamp integer]
        [root_fk integer]
        [method text]
        [filename text]
        [total integer])

      (create-prune-on-insert-trigger 'events 'timestamp 1 10)
      ;; Keep the newest requests.
      (execute (ct:join #\newline
                 "create temporary trigger prune_requests after insert on requests"
                 "begin"
                 "  delete from requests where rowid <= new.rowid - 100000;"
                 "end"))
      (create-index 'events_timestamp "events(timestamp)")

      (create-index 'refs_name "refs(name)")
      (create-index 'refs_uid "refs(uid)")
      (create-index 'refs_root "refs(root_fk)")
      (create-index 'refs_file "refs(file_fk)")
      (create-index 'refs_pre1 "refs(pre1)")
      (create-index 'refs_type "refs(type)"))
    (define (upgrade-db)
      (match (log-db:version schema-name)
        [,@schema-version 'ok]
        [#f
         (log-db:version schema-name schema-version)
         'ok]
        [,version (throw `#(unsupported-db-version ,schema-name ,version))]))

    (match (db:transaction 'log-db
             (lambda ()
               (upgrade-db)
               (init-db)))
      [#(ok ,_)
       (db:expire-cache 'log-db)
       'ignore]
      [,error error]))

  ;; Getting the keywords runs another Scheme process, so it starts after
  ;; the listener and runs in the background. Until it finishes,
  ;; completions just lack keywords.
  (define (tower:load-keywords)
    (spawn
     (lambda ()
       ($update-keywords (erlang:now)
         (get-keywords
          (lambda (reason)
            (do-log 1
              (json:make-object
               [_op_ "get-keywords"]
               [error (exit-reason->english reason)])))))))
    'ignore)

  (define (tower:sup-spec port-number)
    `(#(tower-db:setup ,tower-db:setup temporary 1000 worker)
      #(tower ,tower:start&link permanent 1000 worker)
      ,@(http:configure-server 'http port-number
          (http:url-handler
           (match (<request> path request)
             ["/"
              (let* ([num-clients (tower:num-clients)]
                     [shutdown-time (and (zero? num-clients) (tower:shutdown-time))]
                     [limit (http:find-param "limit" params)]
                     [limit (and limit (string->number limit))]
                     [limit (or limit 20)])
                (match (transaction 'log-db
                         (list
                          (scalar (execute "select count(*) from keywords"))
                          (scalar (execute "select count(*) from refs where type='defn'"))
                          (scalar (execute "select count(*) from refs"))
                          (scalar (execute "select count(*) from files"))
                          (execute "select datetime(timestamp/1000,'unixepoch','localtime'),path from roots order by timestamp desc")
                          (execute "select datetime(timestamp/1000,'unixepoch','localtime'),pid,message from events order by rowid desc limit ?" limit)

                          (execute
                           (ct:join #\space
                             "select method, count(*), min(total), max(total), avg(total), avg(total*total)"
                             "from requests"
                             "group by method"
                             "order by method"))))
                  [(,keywords ,defns ,refs ,files ,roots ,log ,stats)
                   (http:respond conn 200 '(("Content-Type" . "text/html"))
                     (html->bytevector
                      `(html5
                        (head
                         (meta (@ (charset "UTF-8")))
                         (title ,(software-product-name)))
                        (body
                         (pre
                          ,(when shutdown-time
                             (format "Shut down in ~a seconds\n\n"
                               (exact (ceiling (/ (- shutdown-time (erlang:now)) 1000.0)))))
                          ,(format "Connected clients: ~9:D\n" num-clients)
                          ,(format "         Keywords: ~9:D\n" keywords)
                          ,(format "      Definitions: ~9:D\n" defns)
                          ,(format "       References: ~9:D\n" refs)
                          ,(format "     Unique files: ~9:D\n" files))
                         (pre
                          ,@(map
                             (lambda (root)
                               (match root
                                 [#(,date ,path)
                                  (format "~a ~a\n" date path)]))
                             roots))
                         (table (@ (style "text-align:right;"))
                           (thead
                            (tr
                             (th "request")
                             (th "count")
                             (th "min")
                             (th "max")
                             (th "avg")
                             (th "stddev")))
                           (tbody
                            ,@(map
                               (lambda (row)
                                 (match row
                                   [#(,method ,count ,min ,max ,avg ,avg2)
                                    `(tr
                                      (td ,method)
                                      (td ,(format "~:d" count))
                                      (td ,(format "~:d" min))
                                      (td ,(format "~:d" max))
                                      (td ,(format "~,3f" avg))
                                      (td ,(format "~,3f" (stddev count avg avg2))))]))
                               stats)))
                         (pre
                          ,@(map
                             (lambda (row)
                               (match row
                                 [#(,date ,pid ,message)
                                  (format "~a ~a ~a\n" date pid message)]))
                             log))
                         (hr)
                         (pre ,(versions->string))))))]))]
             ["/tower"
              (ws:upgrade conn request (spawn&link client))]
             [,_ #f])))
      #(tower:load-keywords ,tower:load-keywords temporary 1000 worker)))

  (define (tower:start-server verbose tower-db port-number)
    (verbosity (or verbose 0))
    (log-file
     (cond
      [(not tower-db) ":memory:"]
      [(path-absolute? tower-db) tower-db]
      [else (path-combine (base-dir) tower-db)]))
    (app-sup-spec (append (app-sup-spec) (tower:sup-spec port-number)))
    (app:start)
    (receive))
  )
