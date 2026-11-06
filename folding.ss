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

;; Folding ranges come from the indenter's tokens rather than the
;; reader, so they follow the text the editor shows and survive code
;; that doesn't read, such as code being edited.

(library (folding)
  (export
   folding-ranges
   )
  (import
   (chezscheme)
   (indent)
   (swish imports)
   )

  ;; Returns the folding ranges of text as a list of #(start end kind),
  ;; sorted by start. Lines are 0-based and inclusive, and kind is #f,
  ;; "comment" or "imports". These fold:
  ;;
  ;; - a list, vector or bracketed form that spans lines, including one
  ;;   commented out with #;
  ;; - a string that spans lines
  ;; - a block comment that spans lines, and a run of two or more lines
  ;;   that hold only a ; comment
  ;;
  ;; Clients disagree about ranges that start on the same line or that
  ;; overlap without nesting, so only the outermost range on each start
  ;; line is kept, and a range that crosses an earlier one is dropped.
  ;; Given a limit, the ranges that are nested least deeply are kept.
  (define folding-ranges
    (case-lambda
     [(text) (folding-ranges text #f)]
     [(text limit) (normalize (scan (tokenize text)) limit)]))

  (define (count-newlines s)
    (let ([n (string-length s)])
      (do ([i 0 (fx+ i 1)]
           [c 0 (if (eqv? (string-ref s i) #\newline) (fx+ c 1) c)])
          ((fx= i n) c))))

  ;; Whether the first token after the open paren, skipping whitespace
  ;; and comments, is import.
  (define (import-next? ts)
    (match ts
      [() #f]
      [(,t . ,rest)
       (cond
        [(memq (token-type t) '(ws eol line-comment)) (import-next? rest)]
        [else
         (and (eq? (token-type t) 'atomic)
              (eq? (token-value t) 'import))])]))

  ;; Returns the ranges in any order. opens is a stack of the open
  ;; parens' (line0 . kind). block is #f or the (line0 . depth) of the
  ;; outermost open block comment. run is #f or the first line of a run
  ;; of comment lines. code? and comment? say what the current line has
  ;; seen so far.
  (define (scan tokens)
    (let lp ([ts tokens] [line0 0] [opens '()] [block #f] [run #f]
             [code? #f] [comment? #f] [acc '()])
      (define (comment-run end acc)
        (if (and run (fx> end run))
            (cons (vector run end "comment") acc)
            acc))
      (match ts
        [()
         ;; The text may end without a newline after the run.
         (comment-run (if (and comment? (not code?)) line0 (fx- line0 1)) acc)]
        [(,t . ,rest)
         (let ([type (token-type t)])
           ;; Code ends a run of comment lines at the line before.
           (define (next opens block acc)
             (lp rest (fx+ line0 (count-newlines (token-raw t))) opens block
               #f #t comment? (comment-run (fx- line0 1) acc)))
           (cond
            [(eq? type 'eol)
             (if (and comment? (not code?))
                 (lp rest (fx+ line0 1) opens block (or run line0) #f #f acc)
                 (lp rest (fx+ line0 1) opens block #f #f #f
                   (comment-run (fx- line0 1) acc)))]
            [(eq? type 'ws)
             (lp rest line0 opens block run code? comment? acc)]
            [(eq? type 'line-comment)
             (lp rest line0 opens block run code? #t acc)]
            [(eq? type 'lblock-comment)
             (next opens
               (match block
                 [#f (cons line0 1)]
                 [(,start . ,depth) (cons start (fx+ depth 1))])
               acc)]
            [(eq? type 'rblock-comment)
             (match block
               [#f (next opens block acc)]
               [(,start . 1)
                (next opens #f (cons (vector start line0 "comment") acc))]
               [(,start . ,depth)
                (next opens (cons start (fx- depth 1)) acc)])]
            [block (next opens block acc)]
            [(token-open? t)
             (next
              (cons (cons line0 (and (eq? type 'lparen)
                                     (import-next? rest)
                                     "imports"))
                opens)
              block acc)]
            [(token-close? t)
             (match opens
               ;; An extra close paren has nothing to fold.
               [() (next opens block acc)]
               [((,start . ,kind) . ,opens)
                (next opens block (cons (vector start line0 kind) acc))])]
            [(has-prop? t 'string)
             (let ([end (fx+ line0 (count-newlines (token-raw t)))])
               (next opens block (cons (vector line0 end #f) acc)))]
            [else (next opens block acc)]))])))

  (define (range-start r) (vector-ref r 0))
  (define (range-end r) (vector-ref r 1))

  (define (normalize ranges limit)
    (let* ([ranges (filter (lambda (r) (fx> (range-end r) (range-start r))) ranges)]
           ;; By start, and the outermost first among those that start
           ;; on the same line.
           [ranges
            (sort
             (lambda (a b)
               (or (fx< (range-start a) (range-start b))
                   (and (fx= (range-start a) (range-start b))
                        (fx> (range-end a) (range-end b)))))
             ranges)]
           [nested (nest ranges)])
      (map car
        (if (and limit (fx> (length nested) limit))
            (sort
             (lambda (a b) (fx< (range-start (car a)) (range-start (car b))))
             (list-head
              ;; A stable sort keeps the order by start within a depth.
              (list-sort (lambda (a b) (fx< (cdr a) (cdr b))) nested)
              limit))
            nested))))

  ;; Given ranges sorted as normalize sorts them, returns the ranges that
  ;; nest properly, each paired with its depth. enclosing is the stack
  ;; of kept ranges that may enclose the next one.
  (define (nest ranges)
    (let lp ([ranges ranges] [enclosing '()] [last-start #f])
      (match ranges
        [() '()]
        [(,r . ,rest)
         (let ([enclosing
                (let pop ([ls enclosing])
                  (if (and (pair? ls) (fx< (range-end (car ls)) (range-start r)))
                      (pop (cdr ls))
                      ls))])
           (cond
            [(eqv? (range-start r) last-start)
             (lp rest enclosing last-start)]
            [(and (pair? enclosing)
                  (fx> (range-end r) (range-end (car enclosing))))
             ;; r starts inside the enclosing range but ends after it.
             (lp rest enclosing last-start)]
            [else
             (cons (cons r (length enclosing))
               (lp rest (cons r enclosing) (range-start r)))]))])))
  )
