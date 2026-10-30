;;; Copyright 2023 Beckman Coulter, Inc.
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

(library (os-process)
  (export
   os-process:start&link
   os-process:stop
   os-process:write
   )
  (import
   (chezscheme)
   (swish imports)
   )

  ;; The caller is the owner. It's told how the program ended with
  ;; #(os-process-exited pid exit-status term-signal) or
  ;; #(os-process-failed pid reason). Neither kills it.
  (define (os-process:start&link cmd args type get put get-trace)
    (match (gen-server:start&link #f cmd args type get put get-trace self)
      [ignore
       ;; init couldn't start the program.
       (receive
        [#(os-process-start-failed ,reason) `#(error ,reason)])]
      [,result result]))

  (define (os-process:write who data)
    (gen-server:call who `#(write ,data) 'infinity))

  (define (os-process:stop who exit-status)
    (gen-server:call who `#(stop ,exit-status)))

  (define-state-tuple <os-process-state>
    owner                               ; pid
    os-pid
    ip
    op
    ep
    put                                 ; (lambda (op data) ...)
    exit-status
    reader                          ; pid | #f once its input ends
    tracer                          ; pid | #f once its input ends
    ended                           ; #f | #(exit-status term-signal)
    deadline                        ; for reading output after it ends
    )

  (define (init cmd args type get put get-trace owner)
    (define me self)
    (call-with-values
     (lambda () (try (spawn-os-process cmd args me)))
     (case-lambda
      [(fault)
       ;; Exiting normally doesn't kill the linked owner.
       (send owner `#(os-process-start-failed ,fault))
       'ignore]
      [(op ip ep os-pid)
       (let-values ([(ip op ep)
                     (match type
                       [binary (values ip op ep)]
                       [utf8 (values (binary->utf8 ip)
                               (binary->utf8 op)
                               (binary->utf8 ep))])])
         (process-trap-exit #t)
         (unless put
           (force-close-output-port op))
         `#(ok
            ,(<os-process-state> make
               [owner owner]
               [os-pid os-pid]
               [ip ip]
               [op op]
               [ep ep]
               [put put]
               ;; The signal that stops it. On Windows, libuv only
               ;; terminates a process for SIGTERM, SIGKILL, SIGINT or
               ;; SIGQUIT, so it's SIGTERM, not SIGHUP.
               [exit-status 15]
               [reader (spawn&link (lambda () (get ip me)))]
               [tracer (spawn&link (lambda () (get-trace ep me)))]
               [ended #f]
               [deadline #f]
               )))])))

  (define (terminate reason state)
    ($state open [os-pid ip op ep exit-status reader tracer])
    (when os-pid
      (osi_kill* os-pid exit-status))
    (when (memq reason '(normal shutdown))
      (when reader (kill reader 'shutdown))
      (when tracer (kill tracer 'shutdown)))
    (try (close-input-port ip))
    (try (close-input-port ep))
    (force-close-output-port op))

  (define (handle-call msg from state)
    (match msg
      [#(write ,data)
       ($state open [op put])
       (put op data)
       (flush-output-port op)
       `#(reply ok ,state)]
      [#(stop ,exit-status)
       `#(stop normal normal ,($state copy [exit-status exit-status]))]))

  (define (handle-cast msg state) (match msg))

  ;; Once the program has exited, tell the owner after its output has
  ;; been read, or after 1 s if something else keeps the output open.
  ;; The reader's messages to the owner then arrive first.
  (define (continue state)
    ($state open [owner reader tracer ended deadline])
    (cond
     [(not ended) `#(no-reply ,state)]
     [(and (or reader tracer) (< (erlang:now) deadline))
      `#(no-reply ,state ,(- deadline (erlang:now)))]
     [else
      (match ended
        [#(,exit-status ,term-signal)
         (send owner `#(os-process-exited ,self ,exit-status ,term-signal))])
      `#(stop normal ,state)]))

  (define (handle-info msg state)
    (match msg
      [`(EXIT ,from ,reason)
       ($state open [reader tracer])
       (cond
        [(not (or (eq? from reader) (eq? from tracer)))
         ;; The owner stopped, so stop the program.
         `#(stop ,reason ,state)]
        [(eq? reason 'normal)
         ;; Its output ended.
         (continue
          ($state copy
            [reader (and (not (eq? from reader)) reader)]
            [tracer (and (not (eq? from tracer)) tracer)]))]
        [else
         ;; The program's output couldn't be read, e.g. bad JSON.
         (send ($state owner) `#(os-process-failed ,self ,reason))
         `#(stop normal ,state)])]
      [#(process-terminated ,os-pid ,exit-status ,term-signal)
       (continue
        ($state copy
          [os-pid #f]
          [exit-status exit-status]
          [ended `#(,exit-status ,term-signal)]
          [deadline (+ (erlang:now) 1000)]))]
      [timeout (continue state)]))
  )
