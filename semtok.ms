;;; Copyright 2025 Chris Payne
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
 (indent)
 (read)
 (semtok)
 (software-info)
 (testing common)
 )

(software-info:install)

(isolate-mat classifier ()
  (define (test text)
    (let lp ([ls (tokenize text)] [state #f] [acc '()])
      (match ls
        [() (reverse acc)]
        [(,t . ,rest)
         (let-values ([(class mods state) (semtok:classify t state)])
           (lp rest state (cons (list (token-raw t) class (and mods (enum-set->list mods)) state) acc)))])))
  (define (read-exprs filename)
    (let ([ip (open-file-to-read filename)])
      (on-exit (close-port ip)
        (let lp ()
          (let ([x (read ip)])
            (if (eof-object? x)
                '()
                (cons x (lp))))))))
  (define (write-exprs filename ls)
    (let ([op (open-file-to-replace (make-directory-path filename))])
      (on-exit (close-port op)
        (for-each
         (lambda (x)
           (write x op)
           (newline op))
         ls))))
  (define filename "semtok-classifier.ss")
  (define actual-fn (path-combine (actual-dir) filename))

  (delete-file actual-fn)
  (let ([actual (test (utf8->string (read-file (path-combine (support-dir) filename))))]
        [expected (read-exprs (path-combine (expected-dir) filename))])
    (write-exprs actual-fn actual)
    (unless (equal? expected actual)
      (throw 'classifier-mismatch))))

(isolate-mat encode ()
  (match-let*
   ([(0 1 6 2 0 0 7 3 8 0 0 4 2 4 0)
     (semtok:encode "(define foo 12)" 0 (most-positive-fixnum))]
    [(0 0 10 6 0 1 0 10 6 0 1 0 7 6 0)
     (semtok:encode "\"this is a\nmulti-line\nstring\"" 0 (most-positive-fixnum))])
   'ok))
