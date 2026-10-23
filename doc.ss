;;; Copyright 2022 Beckman Coulter, Inc.
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

(library (doc)
  (export
   doc:apply-changes
   doc:line
   doc:read
   doc:source-table
   doc:text
   doc:value-near
   doc:version
   string->doc
   )
  (import
   (chezscheme)
   (json)
   (read)
   (swish imports)
   (trace)
   )

  ;; A doc is one version of one file's text. It is immutable, so it can
  ;; be handed to any process without copying. Derived values are
  ;; computed on first use and cached on the doc, so every consumer of
  ;; a version shares one source table and one read.
  (define-record-type doc
    (nongenerative)
    (sealed #t)
    (fields
     (immutable version)
     (immutable text)
     (mutable source-table)            ; #f | source table
     (mutable read-result))            ; #f | annotated code | a fault
    (protocol
     (lambda (new)
       (lambda (version text source-table)
         (new version text source-table #f)))))

  ;; Processes are preempted, so another process may fill the field
  ;; while compute runs. The long work runs with interrupts enabled;
  ;; only the re-check and store are atomic, and the first value
  ;; stored wins.
  (define (fill! d get put! compute)
    (or (get d)
        (let ([v (compute)])
          (with-interrupts-disabled
           (or (get d)
               (begin (put! d v) v))))))

  (define (string->doc text version)
    (make-doc version text #f))

  (define (doc:text d) (doc-text d))

  (define (doc:version d) (doc-version d))

  (define (doc:source-table d)
    (fill! d doc-source-table doc-source-table-set!
      (lambda () (make-code-lookup-table (doc-text d)))))

  ;; Returns the annotated code or a fault.
  (define (doc:read d)
    (fill! d doc-read-result doc-read-result-set!
      (lambda () (try (read-code (doc-text d))))))

  ;; End of line0's text, before its newline.
  (define (line-end text table line0)
    (let ([next (fx+ line0 1)])
      (if (fx< next (vector-length table))
          (fx- (vector-ref table next) 1)
          (string-length text))))

  (define (doc:line d line0)
    (let ([text (doc-text d)]
          [table (doc:source-table d)])
      (substring text (vector-ref table line0) (line-end text table line0))))

  ;; Clamp the character to the line's length, and a line past the end
  ;; to the end of the text.
  (define (pos->line/fp text table line char)
    (let ([last (fx- (vector-length table) 1)])
      (if (> line last)
          (values last (string-length text))
          (let ([start (vector-ref table line)])
            (values line
              (+ start (min char (- (line-end text table line) start))))))))

  ;; Lines sl through el are replaced by new, which spans sfp to efp in
  ;; the old text. Keep the starts of lines before the edit, add one
  ;; for each newline in new, and shift the starts after the edit.
  (define (splice-table table sl el sfp efp new)
    (let* ([n (vector-length table)]
           [nls (let lp ([i (fx- (string-length new) 1)] [acc '()])
                  (cond
                   [(fx< i 0) acc]
                   [(char=? (string-ref new i) #\newline)
                    (lp (fx- i 1) (cons (fx+ sfp i 1) acc))]
                   [else (lp (fx- i 1) acc)]))]
           [shift (fx- (string-length new) (fx- efp sfp))]
           [tail (fx- n (fx+ el 1))]
           [result (make-vector (fx+ sl 1 (length nls) tail))])
      (do ([i 0 (fx+ i 1)]) ((fx> i sl))
        (vector-set! result i (vector-ref table i)))
      (let lp ([nls nls] [i (fx+ sl 1)])
        (match nls
          [(,fp . ,rest)
           (vector-set! result i fp)
           (lp rest (fx+ i 1))]
          [()
           (do ([j 0 (fx+ j 1)]) ((fx= j tail))
             (vector-set! result (fx+ i j)
               (fx+ (vector-ref table (fx+ el 1 j)) shift)))]))
      result))

  (define (apply-change d version delta)
    (let ([new (json:get delta 'text)]
          [range (json:ref delta 'range #f)])
      (cond
       [range
        (let ([old (doc-text d)]
              [table (doc:source-table d)])
          (let*-values
           ([(sl sfp) (pos->line/fp old table
                        (json:get range '(start line))
                        (json:get range '(start character)))]
            [(el efp) (pos->line/fp old table
                        (json:get range '(end line))
                        (json:get range '(end character)))])
           (let* ([olen (string-length old)]
                  [nlen (string-length new)]
                  [text (make-string (+ sfp nlen (- olen efp)))])
             (string-copy! old 0 text 0 sfp)
             (string-copy! new 0 text sfp nlen)
             (string-copy! old efp text (+ sfp nlen) (- olen efp))
             (make-doc version text (splice-table table sl el sfp efp new)))))]
       [else
        (string->doc new version)])))

  (define (doc:apply-changes d version deltas)
    (fold-left
     (lambda (d delta) (apply-change d version delta))
     d
     deltas))

  (define (doc:value-near d line1 char1)
    (define (->string text value bfp efp)
      (let ([offset (match value
                      [,_ (guard (symbol? value)) 0]
                      [($primitive ,value) 2]
                      [($primitive ,_ ,value) 3]
                      [,_ #f])])
        (and offset
             (substring text (+ bfp offset) efp))))
    (match (try
            (let ([text (doc:line d (fx- line1 1))])
              (let-values ([(type value bfp efp)
                            (read-token-near/col text char1)])
                (and (eq? type 'atomic)
                     (->string text value bfp efp)))))
      [`(catch ,reason)
       (trace-expr
        `(get-value-near ,line1 ,char1 => ,(exit-reason->english reason)))
       #f]
      ["" #f]
      [,result result]))
  )
