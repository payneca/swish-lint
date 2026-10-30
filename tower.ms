;;; Copyright 2026 Chris Payne
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

(import
 (json)
 (tower))

(define (start-tower)
  (log-file ":memory:")
  (match-let*
   ([#(ok ,_)
     (supervisor:start&link 'main-sup 'one-for-all 0 1
       (append
        (make-swish-sup-spec (list swish-event-logger))
        (tower:sup-spec 0)))])
   (http:get-port-number 'http)))

(define (stop-tower)
  (transaction 'log-db 'ok)
  (let ([pid (whereis 'main-sup)])
    (unlink pid)
    (monitor pid)
    (kill pid 'shutdown)
    (receive (after 15000 (throw 'timeout-in-stop))
      [`(DOWN ,_ ,@pid ,_) 'ok])))

(define (update-references id filename refs)
  (json:object->bytevector
   (json:make-object
    [jsonrpc "2.0"]
    [id id]
    [method "update-references"]
    [params
     (json:make-object
      [filename filename]
      [references refs])])))

(define (make-refs prefix n)
  (let lp ([i 0] [acc '()])
    (if (= i n)
        acc
        (lp (+ i 1)
          (cons
           (json:make-object
            [name (format "~a~a" prefix i)]
            [line i]
            [char 0]
            [len 4]
            [meta (json:make-object)])
           acc)))))

(define (file-ref-count filename)
  (transaction 'log-db
    (match (execute "
select count(*) from refs R
inner join files F on F.file_pk=R.file_fk
where F.filename=?" filename)
      [(#(,n)) n])))

(define (connect port)
  (ws:connect "localhost" port "/tower" self))

;; Sends a request and returns the result of its response. Each
;; connection's responses arrive in order, so one id is enough.
(define (tower-call ws method params)
  (ws:send ws
    (json:object->string
     (json:make-object
      [jsonrpc "2.0"]
      [id 1]
      [method method]
      [params params])))
  (receive (after 10000 (throw `#(no-response ,method)))
    [#(ws:message ,@ws ,msg)
     (json:ref
      (if (string? msg)
          (json:string->object msg)
          (json:bytevector->object msg))
      'result #f)]))

(define (reset-directory ws dir)
  (tower-call ws "reset-directory" (json:make-object [directory dir])))

(define (update ws filename refs)
  (tower-call ws "update-references"
    (json:make-object
     [filename filename]
     [references refs])))

(define (ref name line char defn?)
  (json:make-object
   [name name]
   [line line]
   [char char]
   [len (string-length name)]
   [meta
    (if defn?
        (json:make-object [definition 1])
        (json:make-object))]))

(define (position filename line char)
  (json:make-object
   [filename filename]
   [line line]
   [char char]))

;; Returns each location as (filename line char).
(define (locations ls)
  (map
   (lambda (x)
     (list (json:get x 'filename) (json:get x 'line) (json:get x 'char)))
   ls))

(define (completions ws filename line char prefix)
  (map
   (lambda (x) (json:get x 'label))
   (tower-call ws "get-completions"
     (json:make-object
      [filename filename]
      [line line]
      [char char]
      [prefix prefix]))))

;; Returns the status and the content as a string.
(define (http-get port path)
  (let-values ([(ip op) (connect-tcp "localhost" port)])
    (on-exit (begin (close-port op) (close-port ip))
      (put-bytevector op
        (string->utf8
         (format "GET ~a HTTP/1.1\r\nConnection: close\r\n\r\n" path)))
      (flush-output-port op)
      (let* ([status (http:read-status ip 4096)]
             [header (http:read-header ip 1048576)]
             [len (http:get-content-length header)])
        (values status
          (if len
              (utf8->string (get-bytevector-exactly-n ip len))
              ""))))))

(define (home-page port)
  (let-values ([(status content) (http-get port "/")])
    (assert (eqv? status 200))
    content))

;; Waits until the home page reports n connected clients and returns
;; the page. A client registers after its connection is upgraded, so
;; the count can lag.
(define (wait-for-clients port n)
  (let ([pattern (re (format "Connected clients: +~a\n" n))])
    (let lp ([tries 0])
      (let ([page (home-page port)])
        (cond
         [(pregexp-match pattern page) page]
         [(< tries 100)
          (receive (after 10 (lp (+ tries 1))))]
         [else (throw `#(clients-not ,n ,page))])))))

(isolate-mat two-clients ()
  ;; Two clients in different roots share one database. Definitions are
  ;; found in any root, but references and completions stay in the
  ;; client's root.
  (let ([port (start-tower)]
        [a.ss "/tmp/root-a/a.ss"]
        [b.ss "/tmp/root-b/b.ss"])
    (on-exit (stop-tower)
      (let ([a (connect port)]
            [b (connect port)])
        (match-let*
         (["ok" (reset-directory a "/tmp/root-a")]
          ["ok" (reset-directory b "/tmp/root-b")]
          ["ok" (update a a.ss
                  (list
                   (ref "shared" 0 8 #t)
                   (ref "only-a" 1 8 #t)
                   (ref "shared" 2 1 #f)))]
          ["ok" (update b b.ss (list (ref "shared" 3 1 #f)))]
          [((,@a.ss 0 8))
           (locations (tower-call b "get-definitions" (position b.ss 3 2)))]
          [((,@b.ss 3 1))
           (locations (tower-call b "get-references" (position b.ss 3 2)))]
          [((,@a.ss 0 8) (,@a.ss 2 1))
           (locations (tower-call a "get-references" (position a.ss 0 9)))]
          [("only-a") (completions a a.ss 5 0 "only-")]
          [() (completions b b.ss 5 0 "only-")])
         (ws:close a)
         (ws:close b))))))

(isolate-mat missing-root ()
  ;; A client that never sets a root still gets answers about its own
  ;; file.
  (let ([port (start-tower)]
        [fn "/tmp/rootless.ss"])
    (on-exit (stop-tower)
      (let ([ws (connect port)])
        (match-let*
         (["ok" (update ws fn
                  (list
                   (ref "lonely" 0 8 #t)
                   (ref "lonely" 2 1 #f)))]
          [((,@fn 0 8))
           (locations (tower-call ws "get-definitions" (position fn 2 1)))]
          [(,_ ,_)
           (tower-call ws "get-local-references" (position fn 2 1))]
          [,ls (guard (list? ls)) (completions ws fn 4 0 "lone")])
         (ws:close ws))))))

(isolate-mat idle-shutdown ()
  ;; With no clients, the tower schedules its shutdown, and a new client
  ;; cancels it. The mat doesn't wait the 30 seconds for the shutdown.
  (let ([port (start-tower)]
        [shutdown (re "Shut down in (\\d+) seconds")])
    (on-exit (stop-tower)
      (let ([a (connect port)])
        (assert (not (pregexp-match shutdown (wait-for-clients port 1))))
        (ws:close a)
        (match (pregexp-match shutdown (wait-for-clients port 0))
          [(,_ ,seconds)
           (assert (<= 1 (string->number seconds) 30))])
        (let ([b (connect port)])
          (assert (not (pregexp-match shutdown (wait-for-clients port 1))))
          (ws:close b))))))

(define (assert-matches patterns page)
  (for-each
   (lambda (pattern)
     (unless (pregexp-match (re pattern) page)
       (throw `#(missing ,pattern ,page))))
   patterns))

(isolate-mat http-pages ()
  (let ([port (start-tower)]
        [fn "/tmp/root-web/web.ss"])
    (on-exit (stop-tower)
      (let ([ws (connect port)])
        (reset-directory ws "/tmp/root-web")
        (update ws fn (list (ref "page" 0 8 #t) (ref "page" 1 1 #f)))
        (tower-call ws "get-definitions" (position fn 1 1))
        (assert-matches
         '("Definitions: +1\n"
           "References: +2\n"
           "Unique files: +1\n"
           " /tmp/root-web\n"
           "_op_&quot;:&quot;get-definitions&quot;")
         (wait-for-clients port 1))
        (let-values ([(status content) (http-get port "/no-such-page")])
          (assert (eqv? status 404)))
        (ws:close ws)))))

(isolate-mat concurrent-update ()
  ;; Two connections replace the same file's references at once. The
  ;; file must end up with one set, not a mix of both.
  (let ([port (start-tower)]
        [filename "/tmp/concurrent.ss"]
        [n 20000])
    (on-exit (stop-tower)
      (let ([ws1 (ws:connect "localhost" port "/tower" self)]
            [ws2 (ws:connect "localhost" port "/tower" self)]
            [msg1 (update-references 1 filename (make-refs "a" n))]
            [msg2 (update-references 2 filename (make-refs "b" n))])
        (ws:send ws1 msg1)
        (ws:send ws2 msg2)
        (let lp ([replies 0])
          (when (< replies 2)
            (receive
             [#(ws:message ,_ ,_) (lp (+ replies 1))]
             [#(ws:init ,_) (lp replies)])))
        (ws:close ws1)
        (ws:close ws2)
        (assert (= n (file-ref-count filename)))))))
