#!/usr/bin/env swish

(define-syntax (my-thing x)
  (syntax-case x ()
    [(_ abc)
     #'(begin abc)]))

(define-syntax other-thing
  (syntax-rules ()
    [(_ abc)
     (begin abc)]))

(meta define (->spec x)
  body ...)

(meta define ->spec
  (lambda (x)
    body ...))

(define x (lambda (y z) body ...))

(define (x y z) body ...)

11 "this is a \
 long string \
 that is on \
 multiple lines." 14

(re "this is a regexp")

(pregexp-match "a regexp" "this is a string")

(let lp ([x 12]
         [y 13])
  (let ([x y]
        [y x])
    (+ x y)))

(set! foo (#2%car (#3%cdr (#%$enum-set-members))))

(vector-sort! v)
(append! ls)

(#3%vector-sort! abc)

(define-tuple <point> x y)      ; What color should <point> have been?
(<point> make [x 1] [y 2])

(define eol #\newline)
