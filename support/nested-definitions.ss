(library (nested-definitions)
  (export
   )
  (import
   (chezscheme)
   (swish imports)
   )
  (define (inside-library?) #t)

  (define (cobra)
    (define (tomax)
      (+ 1 2))
    (define (xamot)
      (+ 3 4))
    (+ (tomax) (xamot)))

  (module my-module ()
    (define inside-module? #t)
    )

  (define (shrubbery)
    (define (level1)
      (define (level2)
        'little-path)
      (level2))
    (level1))
  )

(define (outside-library?) #t)
