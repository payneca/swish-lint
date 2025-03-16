;;; Copyright 2024 Beckman Coulter, Inc.
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

(library (testing common)
  (export
   actual-dir
   disarm-user-config
   expected-dir
   support-dir
   with-tmp-dir
   write-script
   )
  (import
   (chezscheme)
   (config-params)
   (swish imports)
   (swish testing)
   )
  (define (support-dir) (path-combine (base-dir) "support"))
  (define (expected-dir) (path-combine (support-dir) "mat-expected"))
  (define (actual-dir) (path-combine (support-dir) "mat-actual"))

  (define-syntax with-tmp-dir
    (syntax-rules ()
      [(_ e0 e1 ...)
       (parameterize ([tmp-dir (path-combine (base-dir) "tmp")])
         e0 e1 ...
         (remove-directory (tmp-dir)))]))

  (define-environment-parameters XDG_CONFIG_HOME)

  (define (disarm-user-config)
    (XDG_CONFIG_HOME (support-dir))
    (config:find-files (list (path-combine (support-dir) "no-enumeration"))))

  (define (write-script fn exprs)
    (let ([op (open-file-to-replace (make-directory-path fn))])
      (on-exit (close-port op)
        (fprintf op "#!/usr/bin/env swish\n")
        (for-each (lambda (x) (write x op) (newline op)) exprs)))
    (set-file-mode fn #o777))
  )
