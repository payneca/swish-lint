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
(library (lsp)
  (export
   lsp:find-files
   lsp:read-loop
   lsp:start-server
   lsp:sup-spec
   lsp:write-msg
   )
  (import
   (checkers)
   (chezscheme)
   (config)
   (config-params)
   (doc)
   (indent)
   (json)
   (keywords)
   (progress)
   (read)
   (semtok)
   (software-info)
   (swish imports)
   (tower-client)
   (trace)
   )

  (define (lsp:read-loop ip proc)
    (let lp ()
      (let ([line (get-line ip)])
        (cond
         [(eof-object? line)]
         [(string=? line "") (lp)]
         [else
          (match (pregexp-match (re "\\S+: \\S+") line)
            [(,_) (lp)]
            [#f                      ; we hit the end-of-header marker
             (proc (json:read ip))
             (lp)])]))))

  (define (lsp:start-reader ip proc)
    `#(ok
       ,(spawn&link
         (lambda ()
           (lsp:read-loop ip proc)
           ;; EOF: The client is gone. Let the "exit" handler shutdown
           ;; with the correct status.
           (proc
            (json:make-object
             [method "exit"]))))))

  (define (lsp:write-msg op msg)
    (let* ([bv (json:object->bytevector msg)]
           [str (utf8->string bv)])
      (fprintf op "Content-Length: ~a\r\n\r\n" (bytevector-length bv))
      (display-string str op)))

  (define (lsp:start-writer op)
    `#(ok
       ,(spawn&link
         (lambda ()
           (register 'lsp-writer self)
           (let lp ([timeout 'infinity])
             (receive
              (after timeout
                (flush-output-port op)
                (lp 'infinity))
              [#(send-msg ,msg)
               (lsp:write-msg op msg)
               (lp 1)]))))))

  (define (lsp:send msg)
    (unless (json:object? msg)
      (bad-arg 'lsp:send msg))
    (cond
     [(whereis 'lsp-writer) =>
      (lambda (pid)
        (send pid `#(send-msg ,msg)))]))

  (define (lsp:log type msg)
    (unless (string? msg)
      (bad-arg 'lsp:log msg))
    (rpc:fire-event "window/logMessage"
      (json:make-object
       [type
        (match type
          [error 1]
          [warning 2]
          [info 3]
          [log 4])]
       [message msg])))

  (define (rpc:respond id result)
    (let ([res (json:make-object [jsonrpc "2.0"] [id id])])
      (when result
        (json:extend-object res [result result]))
      (lsp:send res)))

  (define (rpc:request id method params)
    (lsp:send
     (json:make-object
      [jsonrpc "2.0"]
      [id id]
      [method method]
      [params params])))

  (define (rpc:fire-event method params)
    (lsp:send
     (json:make-object
      [jsonrpc "2.0"]
      [method method]
      [params params])))

  (define (rpc:respond-error id code reason)
    (lsp:send
     (json:make-object
      [jsonrpc "2.0"]
      [id id]
      [error
       (json:make-object
        [code code]
        [message (exit-reason->english reason)]
        [data (format "~s" reason)])])))

  (define (make-pos line char)
    (json:make-object [line line] [character char]))

  (define (make-range start end)
    (json:make-object [start start] [end end]))

  (define (make-location uri range)
    (json:make-object [uri uri] [range range]))

  (define (make-text-edit range text)
    (json:make-object [range range] [newText text]))

  (define (make-progress token title render-msg)
    (match (progress:start title render-msg
             (lambda (msg)
               (rpc:fire-event "$/progress"
                 (json:make-object
                  [token token]
                  [value msg])))
             trace-expr)
      [#(ok ,pid) pid]
      [#(error ,reason)
       (trace-expr `(make-progress => ,(exit-reason->english reason)))
       #f]))

  (define (publish-diagnostics uri)
    (rpc:fire-event "textDocument/publishDiagnostics"
      (json:make-object
       [uri uri]
       [diagnostics (current-diagnostics)])))

  (define (start-check uri doc skip-delay?)
    (spawn
     (lambda ()
       (unless skip-delay?
         (receive (after 1000 'ok)))
       (let ([updater (check uri doc skip-delay?)])
         (publish-diagnostics uri)
         (unless skip-delay?
           (receive (after 30000 'ok)))
         (check-line-whitespace (doc:text doc) #t report)
         (publish-diagnostics uri)
         ;; Wait for the reference update. It's linked to this worker,
         ;; so replacing the worker, as closing the file does, then
         ;; cancels it too, and a stale buffer can't overwrite the file's
         ;; references.
         (let ([m (monitor updater)])
           (receive [`(DOWN ,@m ,_ ,_) 'ok]))))))

  (define (start-update-refs uri progress)
    (define pid
      (spawn
       (lambda ()
         (with-gatekeeper-mutex $update-refs 'infinity
           ;; A missing file has no refs. A file that can't be read
           ;; falls back to regular expressions, as check does.
           (let* ([filename (uri->abs-path uri)]
                  [doc (string->doc
                        (if (file-exists? filename)
                            (utf8->string (read-file filename))
                            "")
                        #f)]
                  [annotated-code (match (doc:read doc)
                                    [`(catch ,_) #f]
                                    [,code code])])
             (do-update-refs uri (doc:text doc) annotated-code
               (doc:source-table doc)))))))
    (when progress
      (progress:inc-total progress)
      (spawn
       (lambda ()
         (monitor pid)
         (receive
          [`(DOWN ,_ ,_ ,_)
           (progress:inc-done progress)]))))
    pid)

  (define current-diagnostics (make-process-parameter '()))

  (define (add-diagnostic d)
    (current-diagnostics (cons d (current-diagnostics))))

  (define current-source-table (make-process-parameter #f))

  (define (type->severity type)
    (match type
      [error 1]
      [warning 2]
      [info 3]
      [hint 4]))

  (define (bfp/efp->lsp-range table bfp efp)
    (let-values ([(bl bc) (fp->line/char table bfp)]
                 [(el ec) (fp->line/char table efp)])
      (let ([bl (- bl 1)]               ; LSP is 0-based
            [el (- el 1)]
            [bc (- bc 1)]
            [ec (- ec 1)])
        (make-range (make-pos bl bc) (make-pos el ec)))))

  (define (find-range code table line1 col1)
    (match (try
            (let ([fp (line/char->fp table line1 col1)])
              (let-values ([(type value bfp efp) (read-token-near/fp code fp)])
                (and (eq? type 'atomic)
                     (bfp/efp->lsp-range table bfp efp)))))
      [`(catch ,reason)
       (trace-expr
        `(find-range ,line1 ,col1 => ,(exit-reason->english reason)))
       #f]
      [,result result]))

  (define (->lsp-range x)
    (match x
      [,line (guard (fixnum? line))
       (let ([line (- line 1)])         ; LSP is 0-based
         (make-range (make-pos line 0) (make-pos (+ line 1) 0)))]
      [#(range ,start-line ,start-column ,end-line ,end-column)
       (guard (and (fixnum? start-line) (fixnum? start-column)
                   (fixnum? end-line) (fixnum? end-column)))
       (let ([start-line (- start-line 1)] ; LSP is 0-based
             [end-line (- end-line 1)]
             [start-column (- start-column 1)]
             [end-column (- end-column 1)])
         (make-range (make-pos start-line start-column)
           (make-pos end-line end-column)))]
      [#(near ,code ,line ,column)
       (cond
        [(not line) (->lsp-range 1)]
        [(not column) (->lsp-range line)]
        [(find-range code (current-source-table) line column)]
        [else
         (let ([line (- line 1)]        ; LSP is 0-based
               [column (- column 1)])
           (make-range (make-pos line column) (make-pos (+ line 1) 0)))])]
      [`(annotation [source ,src])
       (bfp/efp->lsp-range (current-source-table)
         (source-object-bfp src)
         (source-object-efp src))]))

  (define (report x type fmt . args)
    (add-diagnostic
     (json:make-object
      [severity (type->severity type)]
      [message (apply format fmt args)]
      [range (->lsp-range x)])))

  ;; Returns the process that updates the file's references.
  (define (check uri doc skip-delay?)
    (define text (doc:text doc))
    (match (doc:read doc)
      [`(catch ,reason)
       (let* ([table (doc:source-table doc)]
              [updater (spawn-update-refs uri #f table text)])
         (let-values ([(line msg) (reason->line/msg reason table)])
           (report line 'error msg))
         updater)]
      [,annotated-code
       (let* ([source-table (doc:source-table doc)]
              [updater (spawn-update-refs uri annotated-code source-table text)])
         (current-source-table source-table)
         (check-import/export annotated-code report)
         (run-optional-checkers uri skip-delay? annotated-code text report)
         updater)]))

  (define (do-update-refs uri text annotated-code source-table)
    (let ([filename (uri->abs-path uri)]
          [refs (make-hashtable string-hash string=?)])
      (define (key name line char)
        (format "~a:~a:~a" name line char))
      (define (try-walk who walk code meta)
        (match
         (try
          (walk code
            (lambda (name uid bfp efp)
              (let-values ([(line char) (fp->line/char source-table bfp)])
                (let ([new (json:make-object
                            ;; For the walkers that pre-computed the
                            ;; string based on bfp/efp, use that
                            ;; string. Otherwise, avoid gensym
                            ;; complications just extract the
                            ;; substring.
                            [name (if (string? name)
                                      name
                                      (substring text bfp efp))]
                            [line line]
                            [char char]
                            [len (- efp bfp)]
                            [meta meta])])
                  ;; Without a uid, the tower uses the name.
                  (when uid
                    (json:set! new 'uid uid))
                  (hashtable-update! refs (key name line char)
                    (lambda (old)
                      (if old
                          (json:merge old new)
                          new))
                    #f))))))
         [`(catch ,reason)
          (trace-expr `(,who => ,(exit-reason->english reason)))
          #f]
         [,_ #t]))
      (define (defns-anno)
        (and annotated-code
             (try-walk 'walk-defns walk-defns annotated-code
               (json:make-object
                [definition 1]
                [anno-pass 1]))))
      (define (defns-re)
        (try-walk 'walk-defns-re walk-defns-re text
          (json:make-object
           [definition 1]
           [regexp-pass 1])))
      (define (refs-anno)
        (and annotated-code
             (try-walk 'walk-refs walk-refs annotated-code
               (json:make-object
                [anno-pass 1]))))
      (define (refs-re)
        (try-walk 'walk-refs-re walk-refs-re text
          (json:make-object
           [regexp-pass 1])))
      (or (defns-anno) (defns-re))
      (or (refs-anno) (refs-re))
      (tower-client:update-references filename
        (vector->list (hashtable-values refs)))
      (event-mgr:notify (cons 'test-sync uri))))

  (define (spawn-update-refs uri annotated-code source-table text)
    (spawn&link
     (lambda ()
       (catch (do-update-refs uri text annotated-code source-table)))))

  (define (get-completions doc uri line char)
    (let ([line (+ line 1)]       ; LSP is 0-based
          #;[char (+ char 1)])    ; ... but we want the preceding char
      (cond
       [(doc:value-near doc line char) =>
        (lambda (prefix)
          (tower-client:get-completions (uri->abs-path uri) line char prefix))]
       [else '()])))

  (define (get-definitions doc uri line char)
    (let ([line (+ line 1)]             ; LSP is 0-based
          [char (+ char 1)])
      (map
       (lambda (defn)
         (let ([uri (abs-path->uri (json:ref defn 'filename #f))]
               [line (- (json:ref defn 'line #f) 1)] ; LSP is 0-based
               [char (- (json:ref defn 'char #f) 1)]
               [len (json:ref defn 'len #f)])
           (make-location uri
             (make-range
              (make-pos line char)
              (make-pos line (+ char len))))))
       (tower-client:get-definitions (uri->abs-path uri) line char))))

  (define (get-references doc uri line char)
    (let ([line (+ line 1)]             ; LSP is 0-based
          [char (+ char 1)])
      (map
       (lambda (ref)
         (let ([uri (abs-path->uri (json:ref ref 'filename #f))]
               [line (- (json:ref ref 'line #f) 1)] ; LSP is 0-based
               [char (- (json:ref ref 'char #f) 1)]
               [len (json:ref ref 'len #f)])
           (make-location uri
             (make-range
              (make-pos line char)
              (make-pos line (+ char len))))))
       (tower-client:get-references (uri->abs-path uri) line char))))

  (define (highlight-references doc uri line char)
    (let ([line (+ line 1)]             ; LSP is 0-based
          [char (+ char 1)])
      (map
       (lambda (ref)
         (let ([line (- (json:ref ref 'line #f) 1)] ; LSP is 0-based
               [char (- (json:ref ref 'char #f) 1)]
               [len (json:ref ref 'len #f)])
           (json:make-object
            [kind 1]                    ; Text
            [range
             (make-range
              (make-pos line char)
              (make-pos line (+ char len)))])))
       (tower-client:get-local-references (uri->abs-path uri) line char))))

  (define (indent-range doc range options)
    (let* ([start (or (and range (json:ref range '(start line) #f)) 0)]
           [end (or (and range (json:ref range '(end line) #f))
                    (fx- (most-positive-fixnum) 1))]
           ;; The end position is exclusive, so a range that ends at
           ;; character 0 does not include that line.
           [end (if (eqv? (and range (json:ref range '(end character) #f)) 0)
                    (- end 1)
                    end)])
      (trace-time 'indent
        (if (< end start)
            '()
            (reverse
             (fold-indent (doc:text doc) (+ start 1) (+ end 1) '()
               (lambda (line old new acc)
                 (let ([line (- line 1)]) ; LSP is 0-based
                   (if (and (<= start line end)
                            (not (string=? old new)))
                       (cons
                        (make-text-edit
                         (make-range
                          (make-pos line 0)
                          (make-pos line (max (string-length old)
                                              (string-length new))))
                         new)
                        acc)
                       acc)))))))))

  (define (format-on-type doc ch line0 char0)
    (trace-time 'format-on-type
      (let* ([text (doc:text doc)]
             [source-table (doc:source-table doc)]
             [line1 (+ line0 1)])
        (let-values ([(start-line1 paren-fp) (find-indent-start text line1)])
          (if (not start-line1)
              ;; line1 is inside a string or comment, so leave it alone.
              '()
              (let* ([line-fp (line/char->fp source-table start-line1 1)]
                     ;; If start-line1 begins inside a string, comment,
                     ;; or |symbol|, start at the enclosing paren, with
                     ;; spaces in place of what precedes it on its line.
                     [start-fp (or paren-fp line-fp)]
                     ;; End of line1, before its newline. The new line
                     ;; may be the last line of the document, so there
                     ;; may be no following newline.
                     [end-fp
                      (if (< line1 (vector-length source-table))
                          (- (line/char->fp source-table (+ line1 1) 1) 1)
                          (string-length text))]
                     ;; Indent only the text from start-line1 through
                     ;; line1. Prior lines are needed for context, but
                     ;; only the intended line should change. A fake
                     ;; character keeps line1 from being blank.
                     [text (string-append
                            (make-string (- start-fp line-fp) #\space)
                            (substring text start-fp end-fp)
                            "a\n")])
                (fold-indent text
                  1 (+ (- line1 start-line1) 1) '()
                  (lambda (line old new acc)
                    ;; LSP is 0-based
                    (let ([line (+ line start-line1 -2)])
                      (if (and (= line line0)
                               (not (string=? old new)))
                          ;; Subtract one from string lengths to eliminate
                          ;; the fake character.
                          (let ([olen (- (string-length old) 1)]
                                [nlen (- (string-length new) 1)])
                            (string-truncate! new nlen)
                            (cons
                             (make-text-edit
                              (make-range
                               (make-pos line 0)
                               (make-pos line (max olen nlen)))
                              new)
                             acc))
                          acc))))))))))

  (define (get-semantic-tokens doc uri range semtok-mode)
    (trace-time 'semantic-tokens
      (match (try
              (let ([start (or (and range (json:ref range '(start line) #f)) 0)]
                    [end (or (and range (json:ref range '(end line) #f)) (fx- (most-positive-fixnum) 1))])
                (semtok:encode (doc:text doc) start end semtok-mode)))
        [`(catch ,reason)
         (trace-expr `(semantic-tokens => ,(exit-reason->english reason)))
         json-null]
        [,data (json:make-object [data data])])))

  (define (get-symbols doc uri)
    (define-tuple <range> bfp efp)
    (define-tuple <doc-symbol>
      name
      range
      outer-range
      children
      )
    (define (range:inside? inner outer)
      (<range> open inner inner. [bfp efp])
      (<range> open outer outer. [bfp efp])
      (and (>= inner.bfp outer.bfp)
           (<= inner.efp outer.efp)))
    (define (symbol:inside? inner outer)
      (range:inside?
       (<doc-symbol> range inner)
       (<doc-symbol> outer-range outer)))
    (define (rebuild-symbols ls)
      ;; pre: ls is ordered inner-most to outer-most.
      (let lp ([ls ls] [roots '()])
        (match ls
          [() roots]
          [(,s . ,rest)
           (let-values ([(children siblings) (partition (lambda (x) (symbol:inside? x s)) roots)])
             (lp rest
               (cons
                (<doc-symbol> copy s
                  [children children])
                siblings)))])))
    (trace-time 'get-symbols
      (match
       (try
        (let* ([text (doc:text doc)]
               [source-table (doc:source-table doc)]
               [annotated-code (match (doc:read doc)
                                 [`(catch ,reason) (raise reason)]
                                 [,code code])]
               [symbols '()])
          (define (range->lsp-range r)
            (<range> open r [bfp efp])
            (bfp/efp->lsp-range source-table bfp efp))
          (define (doc-symbol->lsp s)
            (<doc-symbol> open s [name range outer-range children])
            (json:make-object
             [name name]
             [kind 13]
             [range (range->lsp-range outer-range)]
             [selectionRange (range->lsp-range range)]
             [children (map doc-symbol->lsp children)]))
          (walk-defns+ annotated-code
            (lambda (name uid bfp efp outer.bfp outer.efp)
              (set! symbols
                (cons
                 (<doc-symbol> make
                   [name (substring text bfp efp)]
                   [range (<range> make [bfp bfp] [efp efp])]
                   [outer-range (<range> make [bfp outer.bfp] [efp outer.efp])]
                   [children '()])
                 symbols))))
          (map doc-symbol->lsp (rebuild-symbols symbols))))
       [`(catch ,reason)
        (trace-expr `(get-symbols => ,(exit-reason->english reason)))
        '()]
       [,result result])))

  (define (keep-file? fn)
    (let ([ext (path-extension fn)])
      (or (member ext '("ss" "ms"))
          (let ([ip (open-binary-file-to-read fn)])
            (on-exit (close-port ip)
              (and (eqv? (get-u8 ip) (char->integer #\#))
                   (eqv? (get-u8 ip) (char->integer #\!))
                   (let ([b (get-u8 ip)])
                     (or (eqv? b (char->integer #\space)) (eqv? b (char->integer #\/))))
                   (let ([ip (binary->utf8 ip)])
                     (let ([line (get-line ip)])
                       (and (not (eof-object? line))
                            (pregexp-match (re "swish|scheme|petite") line)
                            #t)))))))))

  (define (get-nul-terminated ip)
    (let ([c (get-char ip)])
      (if (eof-object? c)
          c
          (let-values ([(op get) (open-string-output-port)])
            (let lp ([c c])
              (cond
               [(or (eof-object? c) (char=? c #\nul)) (get)]
               [else
                (write-char c op)
                (lp (get-char ip))]))))))

  ;; Returns the files that cmd lists, one per get-name record, or #f
  ;; if cmd can't be started, fails, or produces no output for timeout
  ;; ms. Unless quiet? is true, cmd's stderr is shown even when not
  ;; tracing.
  (define (find-files-external cmd path get-name timeout quiet?)
    (trace-expr `(find-files-external ,cmd ,path))
    (call-with-values
     (lambda ()
       (try (parameterize ([cd path])
              (spawn-os-process (car cmd) (cdr cmd) self))))
     (case-lambda
      [(fault)
       (trace-expr `(find-files => ,(exit-reason->english fault)))
       #f]
      [(to-stdin from-stdout from-stderr os-pid)
       (let ([me self]
             [to-stdin (binary->utf8 to-stdin)]
             [from-stdout (binary->utf8 from-stdout)]
             [from-stderr (binary->utf8 from-stderr)])
         (define (stop-command reason)
           (trace-expr `(find-files => ,reason))
           (osi_kill* os-pid 15)
           #f)
         ;; kill doesn't run on-exit, so another process stops the
         ;; command if this one is cancelled.
         (spawn
          (lambda ()
            (let ([m (monitor me)])
              (receive
               [`(DOWN ,@m ,_ ,reason)
                (unless (eq? reason 'normal)
                  (try (osi_kill* os-pid 15)))]))))
         (on-exit (begin (close-input-port from-stdout)
                         (close-input-port from-stderr))
           (close-output-port to-stdin)
           (spawn
            (lambda ()
              (let lp ()
                (let ([line (get-line from-stderr)])
                  (unless (eof-object? line)
                    (unless (and quiet? (not trace?))
                      (display line (trace-output-port))
                      (newline (trace-output-port)))
                    (lp))))))
           (spawn
            (lambda ()
              (let lp ()
                (let ([fn (get-name from-stdout)])
                  (send me `#(find-files-output ,os-pid ,fn))
                  (unless (eof-object? fn)
                    (lp))))))
           (let lp ([ls '()])
             (receive (after timeout (stop-command 'timeout))
               [#(find-files-output ,@os-pid ,fn)
                (cond
                 [(eof-object? fn)
                  (receive (after timeout (stop-command 'timeout))
                    [#(process-terminated ,@os-pid ,exit-status ,term-signal)
                     (if (and (eqv? exit-status 0) (eqv? term-signal 0))
                         (reverse ls)
                         (begin
                           (trace-expr
                            `(find-files => #(exit ,exit-status ,term-signal)))
                           #f))])]
                 [else
                  (let ([fn (path-combine path fn)])
                    (if (and (regular-file? fn) (keep-file? fn))
                        (lp (cons fn ls))
                        (lp ls)))])]))))])))

  (define (find-files-default path)
    (filter-files path
      (lambda (dir)
        (cond
         [(string=? (path-last dir) ".git") #f] ; skip the actual repo tree
         [(file-exists? (path-combine dir ".git")) #f] ; skip repos
         [else #t]))
      keep-file?))

  ;; --others --exclude-standard adds untracked files but not ignored
  ;; ones. -z keeps git from quoting unusual file names.
  (define git-ls-files
    '("git" "ls-files" "-z" "--cached" "--others" "--exclude-standard"))

  ;; Try the configured command, then git, then walk the directories.
  ;; Returns which one succeeded (find-files, git, or directory-walk)
  ;; and the files it found. A command that produces no output for
  ;; timeout ms fails.
  (define lsp:find-files
    (case-lambda
     [(path) (lsp:find-files path 10000)]
     [(path timeout)
      (let ([ff (config:find-files)])
        (cond
         [(and ff (find-files-external ff path get-line timeout #f)) =>
          (lambda (files) (values 'find-files files))]
         ;; Outside a repository git fails, which is expected.
         [(find-files-external git-ls-files path get-nul-terminated timeout
            #t) =>
          (lambda (files) (values 'git files))]
         [else (values 'directory-walk (find-files-default path))]))]))

  (define (lsp-server:start&link)
    (define-state-tuple <lsp-server>
      root-uri
      root-dir
      req->pid
      pid->req
      uri->doc
      uri->worker
      indexer                           ; #f | project enumerator
      client-cap
      requests
      semtok-mode
      )
    (define shutdown-requested? #f)

    (define (init)
      `#(ok ,(<lsp-server> make
               [root-uri #f]
               [root-dir #f]
               [req->pid (ht:make equal-hash equal?
                           (lambda (x) (or (fixnum? x) (string? x))))]
               [pid->req (ht:make process-id eq? process?)]
               [uri->doc (ht:make string-hash string=? string?)]
               [uri->worker (ht:make string-hash string=? string?)]
               [indexer #f]
               [client-cap #f]
               [requests (ht:make string-hash string=? string?)]
               [semtok-mode 'full]
               )))
    (define (terminate reason state) 'ok)
    (define (handle-call msg from state)
      (match msg
        [#(incoming-message ,msg)
         (let ([id (json:ref msg 'id #f)])
           (cond
            [(not id)
             (let ([method (json:get msg 'method)]
                   [params (json:ref msg 'params #f)])
               `#(reply ok ,(do-handle-notify method params state)))]
            [(and (string? id) (ht:ref ($state requests) id #f)) =>
             (lambda (callback)
               `#(reply ok
                   ,(callback msg
                      ($state copy* [requests (ht:delete requests id)]))))]
            [(not (json:ref msg 'method #f))
             ;; A response to a request we are not waiting for
             (trace-expr `(unexpected-response ,id))
             `#(reply ok ,state)]
            [else
             (let ([method (json:get msg 'method)]
                   [params (json:ref msg 'params #f)])
               `#(reply ok ,(do-handle-request id method params state)))]))]))
    (define (handle-cast msg state)
      (match msg
        [tower-reconnected
         ;; The tower may be new and empty. Re-check open documents,
         ;; and index the rest of the project from disk.
         `#(no-reply
            ,(index-project
              (ht:fold ($state uri->doc)
                (lambda (uri doc state)
                  (set-worker uri (start-check uri doc #t) state))
                state)))]))
    (define (handle-info msg state)
      (match msg
        [#(request-finished ,pid ,id ,result)
         (rpc:respond id result)
         `#(no-reply ,(delete-req id pid state))]
        [#(project-files ,pid ,files)
         ;; A cancelled enumerator may have sent its files before it
         ;; was killed.
         `#(no-reply
            ,(if (eq? pid ($state indexer))
                 (index-files files ($state copy [indexer #f]))
                 state))]
        [`(DOWN ,_ ,pid ,reason)
         (cond
          [(ht:ref ($state pid->req) pid #f) =>
           (lambda (id)
             (rpc:respond-error id -32000 reason)
             `#(no-reply ,(delete-req id pid state)))]
          [(eq? pid ($state indexer))
           (trace-expr `(index-project => ,(exit-reason->english reason)))
           `#(no-reply ,($state copy [indexer #f]))]
          [else
           `#(no-reply ,state)])]))

    (define (delete-req id pid state)
      ($state copy*
        [req->pid (ht:delete req->pid id)]
        [pid->req (ht:delete pid->req pid)]))

    (define (set-worker uri worker state)
      (let ([old (ht:ref ($state uri->worker) uri #f)])
        (when old (kill old 'cancelled)))
      ($state copy* [uri->worker (ht:set uri->worker uri worker)]))

    ;; uri->doc holds only open documents. Files that are indexed from
    ;; disk have only a worker, which opening the file cancels.
    (define (index-file uri progress state)
      (set-worker uri (start-update-refs uri progress) state))

    ;; Enumerating the project can take a long time, so it runs in its
    ;; own process and sends the files back. Starting a new enumeration
    ;; cancels the old one.
    (define (index-project state)
      (cond
       [($state root-dir) =>
        (lambda (dir)
          (let ([old ($state indexer)])
            (when old (kill old 'cancelled)))
          (let* ([me self]
                 [pid (spawn
                       (lambda ()
                         (let ([start (erlang:now)])
                           (let-values ([(who files) (lsp:find-files dir)])
                             (let ([end (erlang:now)])
                               (trace-expr
                                `(find-files ,who => ,(length files) files in
                                   ,(- end start) ms))
                               (send me `#(project-files ,self ,files)))))))])
            (monitor pid)
            ($state copy [indexer pid])))]
       [else state]))

    ;; An open document's check worker indexes its buffer, which wins
    ;; over the file on disk.
    (define (open-doc? uri state)
      (ht:ref ($state uri->doc) uri #f))

    (define (index-files files state)
      (let ([progress (make-progress "enumerate-directories"
                        "Analyze files"
                        (lambda (done total)
                          (format "~a/~a files" done total)))]
            [uris (filter (lambda (uri) (not (open-doc? uri state)))
                    (map abs-path->uri files))])
        (cond
         [(null? uris)
          ;; This is necessary for testing code that
          ;; depends on the progress events.
          (progress:inc-total progress)
          (progress:inc-done progress)
          state]
         [else
          (fold-left
           (lambda (state uri)
             (index-file uri progress state))
           state
           uris)])))

    (define (updated uri change version skip-delay? state)
      (let* ([old (ht:ref ($state uri->doc) uri #f)]
             [version (or version (and old (doc:version old)))]
             [doc
              (if (string? change)
                  (string->doc change version)
                  (doc:apply-changes (or old (string->doc "" #f)) version
                    change))])
        (set-worker uri (start-check uri doc skip-delay?)
          ($state copy* [uri->doc (ht:set uri->doc uri doc)]))))

    (define (do-handle-request id method params state)
      (match (try (handle-request id method params state))
        [`(catch ,reason)
         (rpc:respond-error id -32000 reason)
         state]
        [#(ok ,result ,state)
         (rpc:respond id result)
         state]
        [#(ignore ,state)
         state]
        [#(spawn ,thunk ,state)
         (let* ([me self]
                [pid (spawn
                      (lambda ()
                        (send me `#(request-finished ,self ,id ,(thunk)))))])
           (monitor pid)
           ($state copy*
             [req->pid (ht:set req->pid id pid)]
             [pid->req (ht:set pid->req pid id)]))]))

    (define (handle-request id method params state)
      (match method
        ["initialize"
         ;;(trace-msg (json:make-object [method method] [params params]))
         (let* ([root-uri (json:get params 'rootUri)]
                [root-uri (and (string? root-uri) root-uri)]
                [root-dir (and root-uri (uri->abs-path root-uri))]
                [client-cap (json:get params 'capabilities)])
           (tower-client:reset-directory root-dir)
           `#(ok
              ,(json:make-object
                [capabilities
                 (json:make-object
                  [textDocumentSync
                   (json:make-object
                    [openClose #t]
                    [change 2]          ; Incremental
                    [willSave #f]
                    [willSaveWaitUntil #f]
                    [save (json:make-object [includeText #t])])]
                  [hoverProvider #f]
                  [completionProvider #t]
                  [definitionProvider #t]
                  [referencesProvider #t]
                  [semanticTokensProvider
                   (json:make-object
                    [range #t]
                    [full #t]
                    [legend
                     (json:make-object
                      [tokenTypes (map symbol->string (semtok:types-list))]
                      [tokenModifiers (map symbol->string (semtok:modifiers-list))])])]
                  [documentHighlightProvider #t]
                  [documentFormattingProvider #t]
                  [documentRangeFormattingProvider #t]
                  [documentOnTypeFormattingProvider
                   ;; VS Code notes:
                   ;; Format On Type must be on.
                   ;; Need a new version of vscode plugin.
                   ;; Need to bind TAB to swish-lint-indent-line
                   (json:make-object
                    [firstTriggerCharacter "\n"]
                    [moreTriggerCharacter (list "\t")] ; Tabs are complicated. :(
                    )]
                  [documentSymbolProvider #t]
                  )])
              ,($state copy
                 [root-uri root-uri]
                 [root-dir root-dir]
                 [client-cap client-cap])))]
        ["textDocument/hover"
         `#(ok #f ,state)]
        ["textDocument/completion"
         (let ([uri (json:get params '(textDocument uri))]
               [line (json:get params '(position line))]
               [char (json:get params '(position character))])
           (cond
            [(ht:ref ($state uri->doc) uri #f) =>
             (lambda (doc)
               `#(spawn ,(lambda () (get-completions doc uri line char)) ,state))]
            [else `#(ok () ,state)]))]
        ["textDocument/definition"
         (let ([uri (json:get params '(textDocument uri))]
               [line (json:get params '(position line))]
               [char (json:get params '(position character))])
           (cond
            [(ht:ref ($state uri->doc) uri #f) =>
             (lambda (doc)
               `#(spawn ,(lambda () (get-definitions doc uri line char)) ,state))]
            [else `#(ok () ,state)]))]
        ["textDocument/references"
         (let ([uri (json:get params '(textDocument uri))]
               [line (json:get params '(position line))]
               [char (json:get params '(position character))])
           (cond
            [(ht:ref ($state uri->doc) uri #f) =>
             (lambda (doc)
               `#(spawn ,(lambda () (get-references doc uri line char)) ,state))]
            [else `#(ok () ,state)]))]
        ["textDocument/documentHighlight"
         (let ([uri (json:get params '(textDocument uri))]
               [line (json:get params '(position line))]
               [char (json:get params '(position character))])
           (cond
            [(ht:ref ($state uri->doc) uri #f) =>
             (lambda (doc)
               `#(spawn ,(lambda () (highlight-references doc uri line char)) ,state))]
            [else `#(ok () ,state)]))]
        ["textDocument/formatting"
         (let ([uri (json:get params '(textDocument uri))]
               [options (json:get params 'options)])
           (cond
            [(ht:ref ($state uri->doc) uri #f) =>
             (lambda (doc)
               `#(spawn ,(lambda () (indent-range doc #f options)) ,state))]
            [else `#(ok () ,state)]))]
        ["textDocument/rangeFormatting"
         (let ([uri (json:get params '(textDocument uri))]
               [range (json:get params 'range)]
               [options (json:get params 'options)])
           (cond
            [(ht:ref ($state uri->doc) uri #f) =>
             (lambda (doc)
               `#(spawn ,(lambda () (indent-range doc range options)) ,state))]
            [else `#(ok () ,state)]))]
        ["textDocument/semanticTokens/range"
         (let ([uri (json:get params '(textDocument uri))]
               [range (json:get params 'range)]
               [semtok-mode ($state semtok-mode)])
           (cond
            [(ht:ref ($state uri->doc) uri #f) =>
             (lambda (doc)
               `#(spawn ,(lambda () (get-semantic-tokens doc uri range semtok-mode)) ,state))]
            [else `#(ok ,json-null ,state)]))]
        ["textDocument/semanticTokens/full"
         (let ([uri (json:get params '(textDocument uri))]
               [semtok-mode ($state semtok-mode)])
           (cond
            [(ht:ref ($state uri->doc) uri #f) =>
             (lambda (doc)
               `#(spawn ,(lambda () (get-semantic-tokens doc uri #f semtok-mode)) ,state))]
            [else `#(ok ,json-null ,state)]))]
        ["textDocument/documentSymbol"
         (let ([uri (json:get params '(textDocument uri))])
           (cond
            [(ht:ref ($state uri->doc) uri #f) =>
             (lambda (doc)
               `#(spawn ,(lambda () (get-symbols doc uri)) ,state))]
            [else `#(ok () ,state)]))]
        ["textDocument/onTypeFormatting"
         (let ([uri (json:get params '(textDocument uri))]
               [ch (json:get params 'ch)]
               ;;[options (json:get params 'options)]
               [line (json:get params '(position line))]
               [char (json:get params '(position character))])
           (cond
            [(ht:ref ($state uri->doc) uri #f) =>
             (lambda (doc)
               `#(spawn ,(lambda () (format-on-type doc (string-ref ch 0) line char)) ,state))]
            [else `#(ok () ,state)]))]
        ["shutdown"
         (set! shutdown-requested? #t)
         `#(ok ,json-null ,state)]
        [,_
         (fprintf (console-error-port) "*** Unhandled message ~s ***\n" method)
         (trace-msg (json:make-object [id id] [method method] [params params]))
         `#(ignore ,state)]))

    (define (do-handle-notify method params state)
      (match (try (handle-notify method params state))
        [`(catch ,reason)
         (trace-expr `(do-handle-notify => ,(exit-reason->english reason)))
         state]
        [,state state]))

    (define (handle-notify method params state)
      (match method
        ["$/cancelRequest"
         (let ([id (json:get params 'id)])
           (cond
            [(ht:ref ($state req->pid) id #f) =>
             (lambda (pid)
               (kill pid 'cancelled)
               (rpc:respond-error id -32800 "Cancelled")
               (delete-req id pid state))]
            [else state]))]
        ["initialized"
         ;; Always load the user configuration event if root-dir is
         ;; not specified.
         (output-env)
         (config:load-user)
         (cond
          [($state root-dir) => config:load-project])
         (index-project state)]
        ["textDocument/didOpen"
         (let ([doc (json:get params 'textDocument)])
           (updated (json:get doc 'uri) (json:get doc 'text)
             (json:ref doc 'version #f) #t state))]
        ["textDocument/didChange"
         (updated
          (json:get params '(textDocument uri))
          (json:get params 'contentChanges)
          (json:ref params '(textDocument version) #f)
          #f
          state)]
        ["textDocument/didSave"
         (updated
          (json:get params '(textDocument uri))
          (json:get params 'text)
          #f
          #t
          state)]
        ["textDocument/didClose"
         ;; The editor has saved or discarded the buffer, so the file on
         ;; disk is the truth. Index it so its refs stay findable.
         (let ([uri (json:get params '(textDocument uri))])
           (index-file uri #f
             ($state copy* [uri->doc (ht:delete uri->doc uri)])))]
        ["workspace/didChangeConfiguration"
         (let* ([semtok-mode (json:ref params '(settings swish semtok-mode) #f)]
                ;; Clients may send settings without our section, or
                ;; with an unknown value. Use the default mode rather
                ;; than one text->semtoks cannot handle.
                [semtok-mode (match semtok-mode
                               ["no-modifiers" 'no-modifiers]
                               [,_ 'full])])
           (cond
            [(eq? semtok-mode ($state semtok-mode))
             state]
            [else
             (let ([id (gensym->unique-string (gensym))])
               (rpc:request id
                 "workspace/semanticTokens/refresh"
                 (json:make-object))
               ($state copy
                 [semtok-mode semtok-mode]
                 [requests
                  (ht:set ($state requests)
                    id
                    (lambda (msg state) state))]))]))]
        ["$/setTrace" state]
        ["exit" (app:shutdown (if shutdown-requested? 0 1))]
        [,_
         (fprintf (console-error-port) "*** Unhandled message ~s ***\n" method)
         (trace-msg (json:make-object [method method] [params params]))
         state]))

    (gen-server:start&link 'lsp-server))

  ;; A message that takes longer than this to handle stops the LSP. It
  ;; means a handler is doing work that belongs in a worker.
  (define (lsp-server:incoming-message msg)
    (gen-server:call 'lsp-server `#(incoming-message ,msg) 5000))

  ;; Crash reports go to the tower's log. Also summarize them on stderr,
  ;; which the editor shows, so that a crash isn't silent.
  (define (report-crash reason)
    (define (describe msg)
      (let ([method (json:ref msg 'method #f)]
            [id (json:ref msg 'id #f)])
        (cond
         [(and method id) (format "~a (request ~s)" method id)]
         [method method]
         [else (format "the response to request ~s" id)])))
    (fprintf (console-error-port) "swish-lint: ~a\n"
      (match reason
        [#(timeout #(gen-server call (lsp-server #(incoming-message ,msg) ,ms)))
         (format "lsp-server took longer than ~a ms to handle ~a"
           ms (describe msg))]
        [,_
         ;; The reason may hold a large message.
         (parameterize ([print-level 4] [print-length 8])
           (exit-reason->english reason))]))
    (flush-output-port (console-error-port)))

  (define (lsp:sup-spec port-number ip op ignore-lhe?)
    `(#(tower-client
        ,(lambda ()
           (tower-client:start&link
            (or port-number (http:get-port-number 'http))
            (lambda ()
              (gen-server:cast 'lsp-server 'tower-reconnected))))
        permanent 1000 worker)
      #(tower-log
        ,(lambda ()
           (match (event-mgr:set-log-handler
                   (lambda (e)
                     (match e
                       [(test-sync . ,_) #f]
                       [`(<child-end> ,reason)
                        (unless (memq reason '(normal shutdown))
                          (report-crash reason))
                        (tower-client:log (coerce e))]
                       [,_ (tower-client:log (coerce e))]))
                   (whereis 'tower-client))
             [ok
              (event-mgr:flush-buffer)
              'ignore]
             [,error
              (if ignore-lhe?
                  'ignore
                  error)]))
        temporary 1000 worker)
      #(lsp-server ,lsp-server:start&link permanent 1000 worker)
      #(lsp:writer
        ,(lambda () (lsp:start-writer op))
        permanent 1000 worker)
      #(lsp:reader
        ,(lambda () (lsp:start-reader ip lsp-server:incoming-message))
        transient 1000 worker)))

  (define (lsp:start-server port-number ip op)
    (trace-init)
    ;; Manually build the whole app-sup-spec. No real need to manage a
    ;; log database or statistics gathering for the LSP client.
    (app-sup-spec
     `(#(event-mgr ,event-mgr:start&link permanent 1000 worker)
       #(gatekeeper ,gatekeeper:start&link permanent 1000 worker)
       ,@(lsp:sup-spec port-number ip op #f)
       #(event-mgr-sentry               ; Should be last
         ,(lambda ()
            `#(ok ,(spawn&link
                    (lambda ()
                      ;; Unregister event-mgr so that event-mgr:notify
                      ;; no longer sends events to tower-log but to
                      ;; the console. LSP is particularly hard to
                      ;; debug during a crash, so more messages to the
                      ;; console, the better.
                      (process-trap-exit #t)
                      (receive
                       [`(EXIT ,_ ,_) (event-mgr:unregister)])))))
         permanent 1000 worker)
       ))
    (fprintf (console-error-port) "~a\n" (versions->string))
    (flush-output-port (console-error-port))
    (app:start)
    (receive))
  )
