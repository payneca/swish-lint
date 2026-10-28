;;; Copyright 2021 Beckman Coulter, Inc.
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
 (config-params)
 (read)
 (software-info))

(software-info:install)

(isolate-mat definitions ()
  (define (extract-name str)
    (let ([result #f])
      (walk-defns-re str
        (lambda (name uid bfp efp)
          (set! result name)))
      result))

  (match-let*
   (["f" (extract-name "(define (f x) x)")]
    ["f" (extract-name "(define f (lambda (x) x))")]
    ["f" (extract-name "(define-syntax f (syntax-rules () ...))")]
    ["f" (extract-name "(define-syntax (f x) (syntax-case x () ...))")]
    ["opt" (extract-name "(define-options opt ...)")]
    [#f (extract-name "(define- f x)")]
    [#f (extract-name "(definegarbage f x)")]
    [#f (extract-name "(define-inline 2 x ...)")]
    ["x" (extract-name "(set! x 5)")]
    ["x" (extract-name "(set-who! x 'me)")]
    [#f (extract-name "(setgarbage! x 'me)")]
    [#f (extract-name "(set-! x 'me)")]
    [#f (extract-name "(set! (x) 5)")]
    [#f (extract-name "(set-who! (x) 'me)")]
    [#f (extract-name "(set!bad x 5)")]
    ["f" (extract-name "(trace-define (f x) x)")]
    ["f" (extract-name "(trace-define-syntax (f x) x)")]
    [#f (extract-name "(trace-trace-define (f x) x)")]
    [#f (extract-name "(tracedefine (f x) x)")]
    ["f" (extract-name "(define-who f x)")]
    ["f" (extract-name "(meta define (f x) x)")]
    ["f" (extract-name "(meta define f (lambda (x) x)")]
    ["f" (extract-name "(meta define-record f (x))")]
    [#f (extract-name "(meta meta define (f x) x)")]
    [#f (extract-name "(meta set! x y)")]
    [#f (extract-name "(field-set! x y)")]
    [#f (extract-name "(set-field! x y)")]
    [#f (extract-name "(set-top-level-value! 'x y)")]
    )
   ;; Check that all (scheme) exports except set! return #f
   (for-each
    (lambda (x)
      (when (and (not (eq? x 'set!))
                 (pregexp-match "set.*!" (symbol->string x)))
        (let ([expr (format "(~a x y)" x)])
          (match (extract-name expr)
            [#f 'ok]
            [,result
             (printf "~a: ~a\n" result expr)
             (throw `#(failed ,result ,expr))]))))
    (library-exports '(scheme)))
   'ok))

(isolate-mat definition-keywords ()
  (define (annotated-defns str)
    (let ([names '()])
      (walk-defns (read-code str)
        (lambda (name uid bfp efp)
          (set! names (cons name names))))
      (reverse names)))
  (define (regexp-defns str)
    (let ([names '()])
      (walk-defns-re str
        (lambda (name uid bfp efp)
          (set! names (cons name names))))
      (reverse names)))
  ;; A configured keyword works like define: alone or with a -suffix,
  ;; but not as the start of a longer name.
  (define text
    (ct:join #\newline
      "(class foo 1)"
      "(class-method (bar) 2)"
      "(classify baz 3)"
      "(defined? qux)"
      "(definer quux)"))
  (parameterize ([config:definition-keywords '("class")])
    (match-let*
     ([(foo bar) (annotated-defns text)]
      [("foo" "bar") (regexp-defns text)])
     'ok))
  ;; A set! form doesn't make later set! forms match the patterns for
  ;; define forms, so (set! (b c) 2) doesn't define b.
  (match-let*
   ([(a) (annotated-defns "(set! a 1)\n(set! (b c) 2)")])
   'ok))

(isolate-mat walk-refs ()
  (define (extract-name str)
    (let ([result #f]
          [annotated-code (read-code str)])
      (walk-refs annotated-code
        (lambda (name uid bfp efp)
          (set! result name)))
      result))

  (match-let*
   ([#f (extract-name "12")]
    [car (extract-name "car")]
    [car (extract-name "(car)")]
    [car (extract-name "#%car")]
    [car (extract-name "#2%car")]
    [car (extract-name "#3%car")]
    [`(catch ,_) (try (extract-name "#4%car"))]
    )
   'ok))

(isolate-mat walk-refs-positions ()
  (define (extract-refs str)
    (let ([result '()])
      (walk-refs (read-code str)
        (lambda (name uid bfp efp)
          (set! result (cons (list name uid bfp efp) result))))
      (reverse result)))
  (define (extract-refs-re str)
    (let ([result '()])
      (walk-refs-re str
        (lambda (name uid bfp efp)
          (set! result (cons (list name uid bfp efp) result))))
      (reverse result)))

  (match-let*
   ;; A primitive reference covers its #% or #N% prefix, and its
   ;; unique id is the name.
   ([((car #f 0 3)) (extract-refs "car")]
    [((car "car" 0 5)) (extract-refs "#%car")]
    [((car "car" 0 6)) (extract-refs "#2%car")]
    [((car "car" 1 7) (x #f 8 9)) (extract-refs "(#3%car x)")]
    ;; The explicit form produces no extra reference for the whole
    ;; expression.
    [(($primitive #f 1 11) (car #f 12 15)) (extract-refs "($primitive car)")]
    [(($primitive #f 1 11) (car #f 14 17))
     (extract-refs "($primitive 2 car)")]
    ;; For the #%, #2%, and #3% prefixes only, the regexp walker reports
    ;; the token text as the name and the identifier as the unique id.
    [(("car" #f 0 3)) (extract-refs-re "car")]
    [(("#%car" "car" 0 5)) (extract-refs-re "#%car")]
    [(("#2%car" "car" 0 6)) (extract-refs-re "#2%car")]
    [(("#3%car" "car" 1 7) ("x" #f 8 9)) (extract-refs-re "(#3%car x)")]
    [(("1%car" #f 1 6)) (extract-refs-re "#1%car")]
    [(("4%car" #f 1 6)) (extract-refs-re "#4%car")]
    [(("%car" #f 2 6)) (extract-refs-re "# %car")])
   'ok))
