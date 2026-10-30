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
 (folding))

(define-syntax code
  (syntax-rules ()
    [(_ str ...)
     (ct:join #\newline str ...)]))

;; Returns each range as (start end) or (start end kind).
(define (folds text . limit)
  (map
   (lambda (r)
     (match r
       [#(,start ,end #f) (list start end)]
       [#(,start ,end ,kind) (list start end kind)]))
   (apply folding-ranges text limit)))

(isolate-mat lists ()
  (match-let*
   ([() (folds "")]
    [() (folds "(f x)\n(g y)\n")]
    ;; Single-line lists don't fold, and the let's binding list starts
    ;; on the let's line, so only the let folds there.
    [((0 4) (1 4) (3 4))
     (folds
      (code
       "(define (f x)"
       "  (let ([a 1]"
       "        [b 2])"
       "    (list a"
       "      b)))"))]
    ;; Vectors and brackets fold too.
    [((0 1) (2 3))
     (folds (code "#(1" "  2)" "[a" " b]"))])
   'ok))

(isolate-mat strings-and-comments ()
  (match-let*
   ([((0 2 "comment") (3 4 "comment") (6 7))
     (folds
      (code
       ";;; one"
       ";;; two"
       ";;; three"
       "#| block"
       "   comment |#"
       "; single"
       "\"two"
       "lines\""))]
    ;; A comment after code ends a run, as does a line of code.
    [((1 2 "comment"))
     (folds
      (code
       "(f) ; trailing"
       "; one"
       "; two"
       "(g)"
       "; alone"))]
    ;; A blank line ends a run too.
    [((0 1 "comment") (3 4 "comment"))
     (folds (code "; a" "; b" "" "; c" "; d"))]
    ;; A run at the end of the text folds, with or without a newline.
    [((1 2 "comment")) (folds (code "(f)" "; a" "; b"))]
    [((1 2 "comment")) (folds (code "(f)" "; a" "; b" ""))]
    ;; Nested block comments fold once, and parens inside a block
    ;; comment or string don't fold.
    [((0 3 "comment"))
     (folds (code "#| outer" "  #| inner" "  |# (a" "  b) |#"))]
    [((0 1)) (folds (code "\"(a" "b)\""))])
   'ok))

(isolate-mat datum-comments ()
  ;; Code commented out with #; still folds.
  (match-let*
   ([((0 3) (1 2))
     (folds
      (code
       "(define (f)"
       "  #;(old"
       "     code)"
       "  (new))"))])
   'ok))

(isolate-mat imports ()
  (match-let*
   ([((0 4) (1 3 "imports"))
     (folds
      (code
       "(library (x)"
       "  (import"
       "   (chezscheme)"
       "   (swish imports))"
       "  body)"))])
   'ok))

(isolate-mat nesting ()
  (match-let*
   (;; The second list starts on the line the first ends, so it crosses
    ;; it and is dropped.
    [((0 1))
     (folds (code "(a" " b) (c" "     d)"))]
    ;; Of ranges that start on one line, the outermost is kept.
    [((0 2)) (folds (code "((a" "  b)" " c)"))])
   'ok))

(isolate-mat unbalanced ()
  ;; Code that doesn't read still folds, except for what an unbalanced
  ;; paren leaves without a match.
  (match-let*
   (;; The first define never closes, but the list inside it does.
    [((0 1) (2 3))
     (folds
      (code
       "(define (f"
       "  x)"
       "(define (g x)"
       "  x)"))]
    [((0 1) (3 4))
     (folds
      (code
       "(a"
       " b)"
       "))"
       "(c"
       " d)"))]
    ;; A string or block comment that doesn't end doesn't fold.
    [((0 1)) (folds (code "(a" " b)" "#| no end" "x"))])
   'ok))

(isolate-mat limit ()
  ;; With a limit, the least nested ranges are kept.
  (let ([text (code
               "(a"
               "  (b"
               "    c))"
               "(d"
               "  (e"
               "    f))")])
    (match-let*
     ([((0 2) (1 2) (3 5) (4 5)) (folds text)]
      [((0 2) (3 5)) (folds text 2)]
      [((0 2) (1 2) (3 5)) (folds text 3)]
      [((0 2) (1 2) (3 5) (4 5)) (folds text 10)])
     'ok)))
