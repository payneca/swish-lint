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
 (os)
 (testing common)
 )

;; Runs a script of exprs with options, removes it, and returns the
;; result.
(define (run-script name exprs . options)
  (let ([fn (path-combine (tmp-dir) name)])
    (write-script fn exprs)
    (on-exit (remove-file/retry fn)
      (let ([cmd (script-command fn)])
        (os:run (car cmd) (cdr cmd)
          (if (null? options) (os:run-options) (car options)))))))

(isolate-mat run-output ()
  (with-tmp-dir
   (let ([exprs '((display "one\ntwo\n") (display "err\n" (current-error-port)))])
     (match-let*
      ([`(<os:result> [stdout ("one" "two")] [stderr ("err")]
           [exit-status 0] [term-signal 0])
        (run-script "lines" exprs)]
       [`(<os:result> [stdout "one\ntwo\n"] [stderr #f])
        (run-script "string" exprs
          (os:run-options [stdout 'string] [stderr #f]))]
       [`(<os:result> [stdout #vu8(111 110 101 10 116 119 111 10)])
        (run-script "bytevector" exprs
          (os:run-options [stdout 'bytevector]))]
       ;; type doesn't affect bytevector, so bytes that aren't UTF-8
       ;; are kept.
       [`(<os:result> [stdout #vu8(255 254 10)])
        (run-script "raw"
          '((let ([op (standard-output-port)])
              (put-bytevector op #vu8(255 254 10))
              (flush-output-port op)))
          (os:run-options [stdout 'bytevector]))]
       [`(<os:result> [stdout "one"] [stderr "err\n"])
        (run-script "utf8" exprs
          (os:run-options
           [stdout get-line]
           [stderr 'string]))]
       [`(<os:result> [stdout 8])
        (run-script "binary" exprs
          (os:run-options
           [type 'binary]
           [stdout (lambda (ip) (bytevector-length (get-bytevector-all ip)))]))])
      'ok))))

(isolate-mat run-input ()
  ;; The program reads stdin to end of file, so the input must be
  ;; closed after it's written.
  (with-tmp-dir
   (let ([exprs '((display (get-string-all (current-input-port))))])
     (match-let*
      ([`(<os:result> [stdout ("hello")])
        (run-script "string" exprs (os:run-options [stdin "hello\n"]))]
       [`(<os:result> [stdout ("utf8")])
        (run-script "utf8" exprs
          (os:run-options
           [stdin (lambda (op) (display "utf8\n" op))]))]
       [`(<os:result> [stdout ("binary")])
        (run-script "binary" exprs
          (os:run-options
           [type 'binary]
           [stdin (lambda (op) (put-bytevector op (string->utf8 "binary\n")))]))]
       [`(<os:result> [stdout ("#!eof")])
        (run-script "none" exprs)])
      'ok))))

(isolate-mat run-cwd ()
  ;; The program starts in cwd, and the caller's directory is unchanged.
  (with-tmp-dir
   (let ([before (cd)]
         [dir (path-combine (tmp-dir) "sub")])
     (make-directory-path (path-combine dir "x"))
     (match-let*
      ([`(<os:result> [stdout (,out)])
        (run-script "pwd" '((display (cd)))
          (os:run-options [cwd dir]))])
      (assert (equal? (get-real-path out) (get-real-path dir)))
      (assert (equal? (cd) before))
      (remove-directory dir)))))

(isolate-mat run-exit ()
  (with-tmp-dir
   (match-let*
    ([`(<os:result> [exit-status 3] [term-signal 0])
      (run-script "exit-3" '((exit 3)))]
     [`(catch #(os-process-failed ,_ `(<os:result> [exit-status 3])))
      (try (run-script "exit-3" '((exit 3))
             (os:run-options [check-exit #t])))]
     [`(catch #(os-process-start-failed ,_ ,_))
      (try (os:run (path-combine (tmp-dir) "dne") '()))])
    'ok)))

;; A script that writes a marker file unless it's killed first.
(define (marker-script marker)
  `((receive (after 2000 'ok))
    (close-port (open-file-to-replace ,marker))))

(define (assert-no-marker marker)
  (receive (after 3000 'ok))
  (assert (not (file-exists? marker))))

(isolate-mat run-timeout ()
  ;; A program that runs too long is killed.
  (with-tmp-dir
   (let ([marker (path-combine (tmp-dir) "finished")])
     (match-let*
      ([`(catch #(os-process-timeout timeout #(stdout ("started")) ,_))
        (try (run-script "slow"
               `((display "started\n") (flush-output-port) ,@(marker-script marker))
               (os:run-options [timeout 500])))])
      (assert-no-marker marker)))))

;; A script that writes a line to stdout or stderr every 250 ms for
;; 2.5 s.
(define (chatter port)
  `((do ([i 0 (+ i 1)]) ((= i 10))
      (display "x\n" ,port)
      (flush-output-port ,port)
      (receive (after 250 'ok)))))

(isolate-mat run-idle-timeout ()
  ;; A program that stops writing is killed.
  (with-tmp-dir
   (let ([marker (path-combine (tmp-dir) "finished")])
     (match-let*
      ([`(catch #(os-process-timeout idle-timeout #(stdout ("started")) ,_))
        (try (run-script "quiet"
               `((display "started\n") (flush-output-port) ,@(marker-script marker))
               (os:run-options [idle-timeout 500])))])
      (assert-no-marker marker)))))

(isolate-mat run-idle-timeout-output ()
  ;; Output on stdout or stderr keeps a program from timing out, though
  ;; it runs longer than idle-timeout.
  (with-tmp-dir
   (match-let*
    ([`(<os:result> [stdout ("x" "x" "x" "x" "x" "x" "x" "x" "x" "x")])
      (run-script "stdout" (chatter '(current-output-port))
        (os:run-options [idle-timeout 1500]))]
     [`(<os:result> [stderr ("x" "x" "x" "x" "x" "x" "x" "x" "x" "x")])
      (run-script "stderr" (chatter '(current-error-port))
        (os:run-options [idle-timeout 1500]))])
    'ok)))

(isolate-mat run-both-timeouts ()
  ;; timeout still limits a program that keeps writing.
  (with-tmp-dir
   (match-let*
    ([`(catch #(os-process-timeout timeout ,_ ,_))
      (try (run-script "chatter" (chatter '(current-output-port))
             (os:run-options [idle-timeout 1500] [timeout 1000])))])
    'ok)))

(isolate-mat run-caller-killed ()
  ;; Killing the caller kills the program.
  (with-tmp-dir
   (let* ([marker (path-combine (tmp-dir) "finished")]
          [pid (spawn (lambda () (run-script "slow" (marker-script marker))))])
     (receive (after 500 (kill pid 'cancelled)))
     (assert-no-marker marker)
     ;; run-script didn't get to remove it.
     (remove-file/retry (path-combine (tmp-dir) "slow")))))

(isolate-mat run-reader-fails ()
  ;; An error reading the output is raised, and kills the program.
  (with-tmp-dir
   (let ([marker (path-combine (tmp-dir) "finished")])
     (match-let*
      ([`(catch bad-output)
        (try (run-script "slow"
               `((display "x\n") (flush-output-port) ,@(marker-script marker))
               (os:run-options
                [stdout (lambda (ip) (get-line ip) (throw 'bad-output))])))])
      (assert-no-marker marker)))))

(isolate-mat run-leaves-no-messages ()
  (with-tmp-dir
   (run-script "quick" '((display "hi\n")))
   (receive (after 0 'ok)
     [,msg (throw `#(unexpected ,msg))])))
