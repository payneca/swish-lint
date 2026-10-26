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
(library (tower-client)
  (export
   abs-path->uri
   tower-client:call
   tower-client:get-completions
   tower-client:get-definitions
   tower-client:get-local-references
   tower-client:get-references
   tower-client:log
   tower-client:reset-directory
   tower-client:shutdown-server
   tower-client:start&link
   tower-client:update-keywords
   tower-client:update-references
   uri->abs-path
   )
  (import
   (chezscheme)
   (swish imports)
   )

  (define (hex-value c)
    (cond
     [(not c) #f]
     [(char<=? #\0 c #\9) (fx- (char->integer c) (char->integer #\0))]
     [(char<=? #\A c #\F) (fx- (char->integer c) (fx- (char->integer #\A) 10))]
     [(char<=? #\a c #\f) (fx- (char->integer c) (fx- (char->integer #\a) 10))]
     [else #f]))

  ;; Decode %XX sequences as UTF-8 bytes. A % that doesn't start a
  ;; valid sequence is kept as is.
  (define (percent-decode s)
    (define n (string-length s))
    (define (char-at i) (and (fx< i n) (string-ref s i)))
    (let-values ([(op get) (open-bytevector-output-port)])
      (let lp ([i 0])
        (cond
         [(fx= i n) (utf8->string (get))]
         [(and (char=? (string-ref s i) #\%)
               (hex-value (char-at (fx+ i 1)))
               (hex-value (char-at (fx+ i 2))))
          (put-u8 op (fx+ (fx* 16 (hex-value (char-at (fx+ i 1))))
                          (hex-value (char-at (fx+ i 2)))))
          (lp (fx+ i 3))]
         [else
          (put-bytevector op (string->utf8 (string (string-ref s i))))
          (lp (fx+ i 1))]))))

  ;; Keep RFC 3986 unreserved characters and the path separator, and
  ;; encode every other UTF-8 byte as %XX.
  (define (percent-encode path)
    (define hex "0123456789ABCDEF")
    (define (keep? b)
      (let ([c (integer->char b)])
        (or (char<=? #\A c #\Z)
            (char<=? #\a c #\z)
            (char<=? #\0 c #\9)
            (memv c '(#\- #\. #\_ #\~ #\/)))))
    (let ([bv (string->utf8 path)]
          [op (open-output-string)])
      (do ([i 0 (fx+ i 1)]) ((fx= i (bytevector-length bv)))
        (let ([b (bytevector-u8-ref bv i)])
          (cond
           [(keep? b) (write-char (integer->char b) op)]
           [else
            (write-char #\% op)
            (write-char (string-ref hex (fxsrl b 4)) op)
            (write-char (string-ref hex (fxand b 15)) op)])))
      (get-output-string op)))

  (define (uri->abs-path uri)
    (match (pregexp-match (re "^file://(.*)") uri)
      [(,_ ,filename)
       (let ([filename (percent-decode filename)])
         (meta-cond
          [windows?
           (pregexp-replace* (re "\\\\")
             (match (pregexp-match (re "^/([A-Za-z]):(.*)") filename)
               [(,_ ,drive ,filename)
                (string-append (string-upcase drive) ":" filename)]
               [,_
                filename])
             "/")]
          [else filename]))]
      [,_ (throw `#(unhandled-uri ,uri))]))

  (define (abs-path->uri path)
    (meta-cond
     [windows?
      (match (pregexp-match (re "^([A-Za-z]):(.*)") path)
        [#f
         ;; When testing it is useful to support Unix paths.
         (string-append "file://"
           (percent-encode (pregexp-replace* (re "\\\\") path "/")))]
        [(,_ ,drive ,filename)
         (string-append "file:///"
           (string-downcase drive)
           "%3A"
           (percent-encode (pregexp-replace* (re "\\\\") filename "/")))])]
     [else
      (string-append "file://" (percent-encode path))]))

  (define (tower-client:start&link port-number)
    (define-state-tuple <tower-client>
      ws
      id
      id-map                            ; id -> from
      sync-token
      )
    (define (try-connect state)
      (match (try (ws:connect "localhost" port-number "/tower" self))
        [`(catch ,_) #f]
        [,ws
         (sync ws state)
         ws]))
    (define (do-connect state)
      (cond
       [($state ws)]
       [(try-connect state)]
       [else
        ;; Explicitly generating the executable's name here. If this
        ;; code is running via Swish script, we want to make sure to
        ;; fire up the Tower correctly.
        (let ([cmd (path-combine (path-parent (app:path)) "swish-lint")])
          (fprintf (trace-output-port) "Launching ~a\n" cmd)
          (spawn-os-process-detached cmd '("--tower"))
          (let lp ([n 1])
            (receive (after 200 'ok))
            (fprintf (trace-output-port) "Reconnecting...\n")
            (cond
             [(try-connect state)]
             [(< n 10) (lp (+ n 1))]
             [else (throw 'unable-to-connect)])))]))
    (define (sync ws state)
      (cond
       [($state sync-token) =>
        (lambda (sync-token)
          (ws:send ws
            (json:object->bytevector
             (json:make-object
              [method "synchronize"]
              [params
               (json:make-object
                [sync-token sync-token])]))))]))
    (define (rpc-reply x)
      (cond
       [(json:ref x 'error #f) =>
        (lambda (err) (make-fault (json:ref err 'message #f)))]
       [else
        (let ([res (json:ref x 'result #!bwp)])
          (when (eq? res #!bwp)
            (throw `#(unhandled-message ,x)))
          res)]))
    (define (init)
      (fprintf (trace-output-port) "Tower URL: http://localhost:~a/\n" port-number)
      `#(ok ,(<tower-client> make
               [ws #f]
               [id 0]
               [id-map (ht:make values eq? fixnum?)]
               [sync-token #f])))
    (define (terminate reason state)
      (ws:close ($state ws)))
    (define (handle-call msg from state)
      (match msg
        [#(call ,msg)
         (match (try (do-connect state))
           [`(catch ,reason ,err)
            `#(reply ,err ,state)]
           [,ws
            (let ([id (+ ($state id) 1)])
              (json:set! msg 'id id)
              (ws:send ws (json:object->bytevector msg))
              `#(no-reply
                 ,($state copy
                    [ws ws]
                    [id id]
                    [id-map (ht:set ($state id-map) id from)])))])]))
    (define (handle-cast msg state)
      (match msg
        [#(cast ,msg)
         (match (try (do-connect state))
           [`(catch ,_)
            `#(no-reply ,state)]
           [,ws
            (ws:send ws (json:object->bytevector msg))
            `#(no-reply ,($state copy [ws ws]))])]))
    (define (handle-info msg state)
      (match msg
        [#(ws:message ,ws ,message)
         (let ([msg (if (string? message)
                        (json:string->object message)
                        (json:bytevector->object message))])
           (match (json:ref msg 'id #f)
             [#f
              (match (json:ref msg 'method #f)
                ["synchronize"
                 `#(no-reply
                    ,($state copy
                       [sync-token (json:ref msg '(params sync-token) #f)]))]
                [,_
                 (fprintf (console-error-port) "Unhandled Tower event\n")
                 (json:write (console-error-port) msg 0)
                 (newline (console-error-port))
                 (flush-output-port (console-error-port))
                 `#(no-reply ,state)])]
             [,id
              (match (ht:ref ($state id-map) id #f)
                [#f
                 (fprintf (console-error-port) "Unexpected message\n")
                 (json:write (console-error-port) msg 0)
                 (newline (console-error-port))
                 (flush-output-port (console-error-port))
                 `#(no-reply ,state)]
                [,from
                 (let ([reply (rpc-reply msg)]
                       [state ($state copy
                                [id-map (ht:delete ($state id-map) id)])])
                   (gen-server:reply from reply)
                   `#(no-reply ,state))])]))]
        [#(ws:closed ,ws ,code ,reason)
         `#(no-reply
            ,(cond
              [(eq? ws ($state ws))
               (ht:fold ($state id-map)
                 (lambda (k v acc)
                   (gen-server:reply v (make-fault 'disconnected))
                   acc)
                 (void))
               ($state copy
                 [ws #f]
                 [id 0]
                 [id-map (ht:make values eq? fixnum?)])]
              [else state]))]
        [#(ws:init ,ws)
         `#(no-reply ,state)]))
    (gen-server:start&link 'tower-client))

  (define (tower-client:call msg)
    (gen-server:call 'tower-client `#(call ,msg)))

  (define (tower-client:cast msg)
    (gen-server:cast 'tower-client `#(cast ,msg)))

  (define (tower-client:get-completions filename line char prefix)
    (tower-client:call
     (json:make-object
      [method "get-completions"]
      [params
       (json:make-object
        [filename filename]
        [line line]
        [char char]
        [prefix prefix])])))

  (define (tower-client:get-definitions filename line char)
    (tower-client:call
     (json:make-object
      [method "get-definitions"]
      [params
       (json:make-object
        [filename filename]
        [line line]
        [char char])])))

  (define (tower-client:get-references filename line char)
    (tower-client:call
     (json:make-object
      [method "get-references"]
      [params
       (json:make-object
        [filename filename]
        [line line]
        [char char])])))

  (define (tower-client:get-local-references filename line char)
    (tower-client:call
     (json:make-object
      [method "get-local-references"]
      [params
       (json:make-object
        [filename filename]
        [line line]
        [char char])])))

  (define (tower-client:log msg)
    (tower-client:cast
     (json:make-object
      [method "log"]
      [params
       (json:make-object
        [timestamp (erlang:now)]
        [message msg])])))

  (define (tower-client:reset-directory dir)
    (tower-client:call
     (json:make-object
      [method "reset-directory"]
      [params
       (json:make-object
        [directory dir])])))

  (define (tower-client:update-keywords keywords)
    ;; Keywords is a list of JSON objects containing: keyword, meta
    (tower-client:call
     (json:make-object
      [method "update-keywords"]
      [params
       (json:make-object
        [keywords keywords])])))

  (define (tower-client:update-references filename refs)
    ;; Refs is a list of JSON objects containing: name, line, char, meta
    (tower-client:call
     (json:make-object
      [method "update-references"]
      [params
       (json:make-object
        [filename filename]
        [references refs])])))

  (define (tower-client:shutdown-server)
    (tower-client:call
     (json:make-object
      [method "shutdown"]
      [params (json:make-object)])))
  )
