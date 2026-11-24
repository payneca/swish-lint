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

(library (keywords)
  (export
   get-keywords
   )
  (import
   (chezscheme)
   (json)
   (os)
   (swish imports)
   )

  (define (read-keyword ip)
    (let ([c (peek-char ip)])
      (cond
       [(eof-object? c) c]
       [(char=? c #\()
        (match (read ip)
          [(,keyword . ,lib)
           (json:make-object
            [keyword (format "~a" keyword)]
            [meta (json:make-object [library (format "~s" lib)])])])]
       [(char=? c #\{)
        (let ([obj (json:read ip)])
          (unless (json:ref obj 'keyword #f)
            (throw `#(invalid-input ,obj)))
          (unless (json:ref obj 'meta #f)
            (json:set! obj 'meta (json:make-object)))
          obj)]
       [else
        (let ([keyword (trim-whitespace (get-line ip))])
          (if (string=? keyword "")
              (read-keyword ip)
              (json:make-object
               [keyword keyword]
               [meta (json:make-object)])))])))

  (define (read-keywords ip ht)
    (let lp ()
      (let ([x (read-keyword ip)])
        (unless (eof-object? x)
          (let ([kw (json:ref x 'keyword #f)])
            (unless kw
              (throw `#(invalid-input ,x)))
            (hashtable-update! ht kw
              (lambda (old)
                (if old
                    (json:merge old x)
                    x))
              #f)
            (lp))))))

  (define (static-keywords ht)
    (define static (path-combine (base-dir) "static-keywords"))
    (when (regular-file? static)
      (let ([ip (open-file-to-read static)])
        (on-exit (close-port ip)
          (read-keywords ip ht)))))

  (define (generate-keywords-expr libs)
    `(for-each
      (lambda (lib)
        (for-each
         (lambda (export)
           (printf "(~s . ~s)\n" export lib))
         (library-exports lib)))
      ,libs))

  (define (generate-keywords ht report-error)
    (let f ([exes '((("swish" "-q") (scheme) ,@(library-requirements '(swish imports)))
                    (("scheme" "-q") (scheme))
                    (("petite" "-q") (scheme)))])
      (match exes
        [()
         (read-keywords
          (open-input-string
           (with-output-to-string
            (lambda ()
              (eval (generate-keywords-expr '(quote ((scheme))))))))
          ht)]
        [(((,exe . ,args) . ,libs) . ,rest)
         (match
          (try
           (os:run exe args
             (os:run-options
              [stdin (format "~s\n" (generate-keywords-expr `(,'quasiquote ,libs)))]
              [stdout 'string]
              [stderr #f]
              [timeout 10000]
              [check-exit #t])))
          [`(catch ,reason)
           (report-error reason)
           (f rest)]
          [`(<os:result> ,stdout)
           (read-keywords (open-input-string stdout) ht)])])))

  (define (get-keywords report-error)
    (match
     (try
      (let ([ht (make-hashtable string-hash string=?)])
        (generate-keywords ht report-error)
        (static-keywords ht)
        (vector->list (hashtable-values ht))))
     [`(catch ,_ ,err)
      (report-error err)
      '()]
     [,keywords keywords]))
  )
