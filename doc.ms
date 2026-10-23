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
 (doc)
 (json)
 (read)
 (software-info))

(software-info:install)

(define (make-pos line char)
  (json:make-object [line line] [character char]))

(define (make-range start end)
  (json:make-object [start start] [end end]))

(define (edit doc text start end)
  (doc:apply-changes doc #f
    (list
     (json:make-object
      [text text]
      [range (make-range start end)]))))

(define (check-doc doc expected)
  (unless (equal? (doc:text doc) expected)
    (throw `#(mismatch ,expected ,(doc:text doc))))
  (unless (equal? (doc:source-table doc) (make-code-lookup-table expected))
    (throw `#(mismatched-source-table ,(doc:source-table doc)
               ,(make-code-lookup-table expected)))))

(isolate-mat edit-line ()
  (define (perform-edits ls)
    (match-let*
     ([,start (string-append "\n" (make-string (+ (apply max ls) 1) #\*) "\n")]
      [,doc (string->doc start 1)]
      [,_ (check-doc doc start)]
      [,doc
       (fold-left
        (lambda (doc n)
          (edit doc (format "~d" n) (make-pos 1 n) (make-pos 1 (+ n 1))))
        doc
        ls)]
      [,_ (check-doc doc (format "\n~{~a~}\n" (sort < ls)))])
     'ok))
  (perform-edits '(0 1 2 3 4 5))
  (perform-edits '(5 4 3 2 1 0))
  (perform-edits '(0 3 2 5 1 4)))

(isolate-mat edit-lines ()
  (define (perform-edits ls)
    (match-let*
     ([,start (format "~{*** ~a ***\n~}" (iota (+ (apply max ls) 1)))]
      [,doc (string->doc start 1)]
      [,_ (check-doc doc start)]
      [,doc
       (fold-left
        (lambda (doc n)
          (edit doc (format "~d\n" n) (make-pos n 0) (make-pos (+ n 1) 0)))
        doc
        ls)]
      [,_ (check-doc doc (format "~{~a\n~}" (sort < ls)))])
     'ok))
  (perform-edits '(0 1 2 3 4 5))
  (perform-edits '(5 4 3 2 1 0))
  (perform-edits '(0 3 2 5 1 4)))

(isolate-mat insert-multiple-lines ()
  (define (perform-edits ls)
    (match-let*
     ([,start (format "~{*** ~a ***\n~}" (iota (* (+ (apply max ls) 1) 2)))]
      [,doc (string->doc start 1)]
      [,_ (check-doc doc start)]
      [,doc
       (fold-left
        (lambda (doc n)
          (edit doc (format "~d\nbonus\n" n)
            (make-pos (* n 2) 0)
            (make-pos (+ (* n 2) 2) 0)))
        doc
        ls)]
      [,_ (check-doc doc (format "~{~a\nbonus\n~}" (sort < ls)))])
     'ok))
  (perform-edits '(0 1 2 3 4 5))
  (perform-edits '(5 4 3 2 1 0))
  (perform-edits '(0 3 2 5 1 4)))

(isolate-mat basic ()
  (match-let*
   ([,start "(let ()\n  (+ x y))\n"]
    [,doc (string->doc start 1)]
    [,_ (check-doc doc start)]
    [,doc (edit doc "a" (make-pos 0 0) (make-pos 0 0))]
    [,_ (check-doc doc (string-append "a" start))]
    [,doc (edit doc "" (make-pos 0 0) (make-pos 0 1))]
    [,_ (check-doc doc start)]
    [,doc (edit doc "" (make-pos 0 4) (make-pos 1 9))]
    [,_ (check-doc doc "(let)\n")]
    [,doc (edit doc " ()\n  (+ x y)" (make-pos 0 4) (make-pos 0 4))]
    [,doc (edit doc " ()\n  (+ x y)" (make-pos 0 4) (make-pos 1 9))]
    [,doc (edit doc ")" (make-pos 1 8) (make-pos 1 9))]
    [,_ (check-doc doc start)]
    [,doc (edit doc "abc\n" (make-pos 2 0) (make-pos 2 0))]
    [,_ (check-doc doc (string-append start "abc\n"))]
    [,doc (edit doc "" (make-pos 2 0) (make-pos 2 3))]
    [,_ (check-doc doc (string-append start "\n"))]
    [,doc (edit doc "" (make-pos 2 0) (make-pos 3 0))]
    [,_ (check-doc doc start)]
    ;; Positions past the end of a line or the text are clamped.
    [,doc (edit doc "!" (make-pos 0 99) (make-pos 0 99))]
    [,_ (check-doc doc "(let ()!\n  (+ x y))\n")]
    [,doc (edit doc "" (make-pos 0 7) (make-pos 0 8))]
    [,doc (edit doc "z" (make-pos 9 0) (make-pos 9 0))]
    [,_ (check-doc doc (string-append start "z"))]
    [,doc (edit doc "" (make-pos 2 0) (make-pos 2 1))]
    [,_ (check-doc doc start)]
    [,doc (doc:apply-changes doc 2 (list (json:make-object [text ""])))]
    [,_ (check-doc doc "")]
    [,doc (doc:apply-changes doc 3 (list (json:make-object [text "abc\n"])))]
    [,_ (check-doc doc "abc\n")]
    [3 (doc:version doc)])
   'ok))

(isolate-mat multiple-changes ()
  (match-let*
   ([,doc (string->doc "a\nb\nc\n" 1)]
    [,doc (doc:apply-changes doc 2
            (list
             (json:make-object
              [text "x\ny"]
              [range (make-range (make-pos 1 0) (make-pos 1 1))])
             (json:make-object
              [text ""]
              [range (make-range (make-pos 0 0) (make-pos 1 0))])))]
    [,_ (check-doc doc "x\ny\nc\n")]
    [2 (doc:version doc)]
    ["y" (doc:line doc 1)]
    ["" (doc:line doc 3)])
   'ok))

(isolate-mat cached ()
  (match-let*
   ([,doc (string->doc "(define (f x) x)\n" 1)]
    [,code (doc:read doc)]
    [#t (eq? code (doc:read doc))]
    [,table (doc:source-table doc)]
    [#t (eq? table (doc:source-table doc))]
    [,bad (string->doc "(define (f x)\n" 1)]
    [`(catch ,_) (doc:read bad)]
    [#t (eq? (doc:read bad) (doc:read bad))])
   'ok))

(isolate-mat value-near ()
  (match-let*
   ([,doc (string->doc "(let ()\n  (list-head x y))\n" 1)]
    ["list-head" (doc:value-near doc 2 5)]
    [#f (doc:value-near doc 2 1)]
    [#f (doc:value-near doc 9 1)])
   'ok))

(isolate-mat random-edits ()
  (define alphabet "ab\n")
  (define (random-text n)
    (let ([s (make-string n)])
      (do ([i 0 (+ i 1)]) ((= i n) s)
        (string-set! s i (string-ref alphabet (random 3))))))
  (define (random-pos text)
    (let* ([table (make-code-lookup-table text)]
           [line (random (vector-length table))]
           [start (vector-ref table line)]
           [end (if (< (+ line 1) (vector-length table))
                    (- (vector-ref table (+ line 1)) 1)
                    (string-length text))])
      (cons line (random (+ (- end start) 1)))))
  (define (->fp text pos)
    (+ (vector-ref (make-code-lookup-table text) (car pos)) (cdr pos)))
  (let lp ([i 0] [text (random-text 40)] [doc #f])
    (let ([doc (or doc (string->doc text 1))])
      (check-doc doc text)
      (when (< i 500)
        (let* ([a (random-pos text)]
               [b (random-pos text)]
               [a.fp (->fp text a)]
               [b.fp (->fp text b)]
               [start (if (<= a.fp b.fp) a b)]
               [end (if (<= a.fp b.fp) b a)]
               [new (random-text (random 6))])
          (lp (+ i 1)
            (string-append (substring text 0 (min a.fp b.fp)) new
              (substring text (max a.fp b.fp) (string-length text)))
            (edit doc new (make-pos (car start) (cdr start))
              (make-pos (car end) (cdr end)))))))))
