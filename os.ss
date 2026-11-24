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

(library (os)
  (export
   <os:result>
   os:run
   os:run-options
   )
  (import
   (chezscheme)
   (swish imports)
   )

  (define-tuple <os:result> stdout stderr exit-status term-signal)

  (define (input? x)
    (or (not x) (string? x) (bytevector? x) (procedure? x)))

  (define (output? x)
    (or (not x) (memq x '(lines string bytevector)) (procedure? x)))

  (define (type? x) (memq x '(binary utf8)))

  (define (timeout? x)
    (or (eq? x 'infinity) (and (fixnum? x) (fxnonnegative? x))))

  ;; type is utf8 or binary, the kind of port a procedure for stdin,
  ;; stdout or stderr is given. The procedure doesn't close it. Use utf8
  ;; rather than calling binary->utf8 on a binary port: that marks the
  ;; binary port closed, so closing it no longer closes the pipe, and the
  ;; program may never see end of file. type doesn't affect the other
  ;; forms: lines and strings are UTF-8, and bytevectors are bytes.
  ;;
  ;; stdin is what the program reads: a string, a bytevector, or a
  ;; procedure that writes to the port it's given. With #f, the program
  ;; reads end of file.
  ;;
  ;; stdout and stderr say how to collect each stream: as a list of
  ;; lines, a string, a bytevector, or the value of a procedure that
  ;; reads the port it's given. With #f, the stream is read and
  ;; dropped.
  ;;
  ;; timeout limits the whole run, and idle-timeout the time without
  ;; output on stdout or stderr, both in ms.
  ;;
  ;; cwd is the program's working directory, or #f for the current one.
  (define-options os:run-options
    (optional
     [cwd (default #f) (must-be (lambda (x) (or (not x) (string? x))))]
     [type (default 'utf8) (must-be type?)]
     [stdin (default #f) (must-be input?)]
     [stdout (default 'lines) (must-be output?)]
     [stderr (default 'lines) (must-be output?)]
     [timeout (default 'infinity) (must-be timeout?)]
     [idle-timeout (default 'infinity) (must-be timeout?)]
     [check-exit (default #f) (must-be boolean?)]))

  (define default-run-options (os:run-options))

  (define (start path args cwd me)
    ;; cd is shared by every Swish process, so it changes only around
    ;; spawn-os-process, which doesn't yield.
    (define (spawn-it) (spawn-os-process path args me))
    (call-with-values
     (lambda () (try (if cwd (parameterize ([cd cwd]) (spawn-it)) (spawn-it))))
     (case-lambda
      [(fault)
       (match fault
         [`(catch ,reason) (throw `#(os-process-start-failed ,path ,reason))])]
      [(to-stdin from-stdout from-stderr os-pid)
       (values to-stdin from-stdout from-stderr os-pid)])))

  (define (read-output how ip)
    (match how
      [#f
       (let lp ()
         (unless (eof-object? (get-bytevector-some ip))
           (lp)))
       #f]
      [lines
       (let lp ([acc '()])
         (let ([line (get-line ip)])
           (if (eof-object? line)
               (reverse acc)
               (lp (cons line acc)))))]
      [string
       (let ([s (get-string-all ip)])
         (if (eof-object? s) "" s))]
      [bytevector
       (let ([bv (get-bytevector-all ip)])
         (if (eof-object? bv) #vu8() bv))]
      [,proc (proc ip)]))

  ;; The program may exit without reading all of its input, so failing
  ;; to write is not an error.
  (define (feed stdin op)
    (match
     (try
      (cond
       [(not stdin) (void)]
       [(procedure? stdin) (stdin op)]
       [(string? stdin) (put-bytevector op (string->utf8 stdin))]
       [else (put-bytevector op stdin)])
      (flush-output-port op)
      (close-port op))
     [`(catch ,_) (force-close-output-port op)]
     [,_ (void)]))

  (define (run path args options caller)
    (match-define
     `(<os:run-options> ,cwd ,type ,stdin ,stdout ,stderr ,timeout
        ,idle-timeout ,check-exit)
     options)
    (define utf8? (eq? type 'utf8))
    (define (text? how)
      (or (memq how '(lines string)) (and utf8? (procedure? how))))
    ;; The readers set this after each read of stdout or stderr, and the
    ;; idle timeout counts from it. A shared variable avoids a message
    ;; for every read.
    (define shared-read-timestamp (erlang:now))
    (define (watch bp)
      (if (eq? idle-timeout 'infinity)
          bp
          (make-custom-binary-input-port (port-name bp)
            (lambda (bv start n)
              (let ([count (get-bytevector-some! bp bv start n)])
                (cond
                 [(eof-object? count) 0]
                 [else
                  (set! shared-read-timestamp (erlang:now))
                  count])))
            #f #f
            (lambda () (close-port bp)))))
    (define (prepare how bp)
      (let ([ip (watch bp)])
        (if (text? how) (binary->utf8 ip) ip)))
    (define me self)
    (define (collect tag how ip)
      (spawn&link
       (lambda ()
         (on-exit (close-port ip)
           (send me `#(,tag ,(read-output how ip)))))))
    (monitor caller)
    (process-trap-exit #t)
    (let-values ([(to-stdin from-stdout from-stderr os-pid)
                  (start path args cwd me)])
      (let ([to-stdin (if (and utf8? (procedure? stdin))
                          (binary->utf8 to-stdin)
                          to-stdin)]
            [from-stdout (prepare stdout from-stdout)]
            [from-stderr (prepare stderr from-stderr)])
        (define feeder (spawn&link (lambda () (feed stdin to-stdin))))
        (define deadline
          (if (eq? timeout 'infinity) 'infinity (+ (erlang:now) timeout)))
        (define (stop-program)
          (osi_kill* os-pid SIGKILL)
          (receive (after 1000 #f)
            [#(process-terminated ,@os-pid ,_ ,_) #t]))
        (define (idle-deadline)
          (if (eq? idle-timeout 'infinity)
              'infinity
              (+ shared-read-timestamp idle-timeout)))
        (define (expired? t) (and (not (eq? t 'infinity)) (>= (erlang:now) t)))
        (define (next-wake)
          (let ([idle (idle-deadline)])
            (cond
             [(eq? deadline 'infinity) idle]
             [(eq? idle 'infinity) deadline]
             [else (min deadline idle)])))
        (define (timed-out which exit out err)
          (unless exit (stop-program))
          (let ([out (if (eq? out 'pending)
                         (receive (after 100 #f) [#(stdout ,v) v])
                         out)]
                [err (if (eq? err 'pending)
                         (receive (after 100 #f) [#(stderr ,v) v])
                         err)])
            (throw `#(os-process-timeout ,which #(stdout ,out) #(stderr ,err)))))
        (collect 'stdout stdout from-stdout)
        (collect 'stderr stderr from-stderr)
        ;; The readers close their ports when they finish. If this stops
        ;; first, as after a timeout, a reader may still be blocked,
        ;; for example on output that a program the child started
        ;; holds open.
        (on-exit (begin (try (close-port from-stdout))
                        (try (close-port from-stderr)))
          (let lp ([exit #f] [out 'pending] [err 'pending])
            (cond
             [(and exit (not (eq? out 'pending)) (not (eq? err 'pending)))
              (kill feeder 'shutdown)
              (let ([result
                     (<os:result> make
                       [stdout out]
                       [stderr err]
                       [exit-status (car exit)]
                       [term-signal (cdr exit)])])
                (if (and check-exit
                         (not (and (eqv? (car exit) 0) (eqv? (cdr exit) 0))))
                    (throw `#(os-process-failed ,path ,result))
                    result))]
             [else
              (receive (until (next-wake)
                         (cond
                          [(expired? deadline)
                           (timed-out 'timeout exit out err)]
                          [(expired? (idle-deadline))
                           (timed-out 'idle-timeout exit out err)]
                          [else (lp exit out err)]))
                [#(process-terminated ,@os-pid ,exit-status ,term-signal)
                 (lp (cons exit-status term-signal) out err)]
                [#(stdout ,v) (lp exit v err)]
                [#(stderr ,v) (lp exit out v)]
                [`(DOWN ,_ ,@caller ,_)
                 (unless exit (stop-program))
                 (throw 'caller-exited)]
                [`(EXIT ,_ normal) (lp exit out err)]
                [`(EXIT ,_ ,_ ,e)
                 (unless exit (stop-program))
                 (raise e)])]))))))

  ;; Runs a program to completion and returns an <os:result>. Raises
  ;; #(os-process-start-failed path reason) if the program can't be
  ;; started, #(os-process-timeout which #(stdout out) #(stderr err)) if
  ;; it runs longer than timeout ms or has no output for idle-timeout ms,
  ;; where which is timeout or idle-timeout, and #(os-process-failed
  ;; path result) if check-exit is true and it fails. An error in a
  ;; procedure for stdout or stderr is raised too. In each case, and if
  ;; the caller exits, the program is killed. Programs it started are
  ;; not.
  (define os:run
    (case-lambda
     [(path args) (os:run path args default-run-options)]
     [(path args options)
      (arg-check 'os:run
        [path string?]
        [args list? (lambda (x) (for-all string? x))]
        [options (os:run-options is?)])
      ;; The program's exit and its output arrive as messages. A worker
      ;; receives them, not the caller, so that when a run fails or
      ;; times out, those that arrive late aren't left in the caller's
      ;; mailbox.
      (let* ([caller self]
             [worker
              (spawn
               (lambda ()
                 (send caller `#(os:run ,self ,(run path args options caller)))))]
             [m (monitor worker)])
        (receive
         [#(os:run ,@worker ,result)
          (demonitor&flush m)
          result]
         [`(DOWN ,@m ,_ ,_ ,e) (raise e)]))]))
  )
