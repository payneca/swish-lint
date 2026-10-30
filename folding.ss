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
   <fold>
   folding-ranges
   )
  (import
   (chezscheme)
   (indent)
   (swish imports)
   )

  (define-tuple <fold> start-line0 start-char0 end-line0 end-char0 kind)

  ;; Returns the folding ranges of text as a list of <fold>, sorted by
  ;; start. Lines are inclusive, and the chars mark the text that folds: from
  ;; just after an open paren, string quote or #| to just before its
  ;; match. A char of #f is the end of the line. kind is #f, "comment" or
  ;; "imports". These fold:
  ;;
  ;; - a list, vector or bracketed form that spans lines, including one
  ;;   commented out with #;
  ;; - a string that spans lines
  ;; - a block comment that spans lines, and a run of two or more lines
  ;;   that hold only a ; comment
  ;;
  ;; With line-only?, as for clients that fold whole lines, the chars
  ;; are ignored. Those clients disagree about ranges that start on the
  ;; same line or that overlap without nesting, so only the outermost
  ;; range on each start line is kept, and a range that crosses an
  ;; earlier one is dropped. Without line-only?, ranges nest by char and
  ;; all are kept. Given a limit, the ranges that are nested least
  ;; deeply are kept.
  (define folding-ranges
    (case-lambda
     [(text) (folding-ranges text #f #t)]
     [(text limit) (folding-ranges text limit #t)]
     [(text limit line-only?)
      (normalize (scan (tokenize text)) limit line-only?)]))

  (define (count-newlines s)
    (let ([n (string-length s)])
      (do ([i 0 (fx+ i 1)]
           [c 0 (if (eqv? (string-ref s i) #\newline) (fx+ c 1) c)])
          ((fx= i n) c))))

  ;; The char just after s, given the char where it starts.
  (define (char0-after char0 s)
    (let lp ([i (fx- (string-length s) 1)])
      (cond
       [(fx< i 0) (fx+ char0 (string-length s))]
       [(eqv? (string-ref s i) #\newline) (fx- (string-length s) (fx+ i 1))]
       [else (lp (fx- i 1))])))

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
  ;; parens' (line0 char0 kind), with the position just after the open
  ;; token. block is #f or the (line0 char0 depth) of the outermost open
  ;; block comment. run is #f or the first line of a run of comment
  ;; lines. code? and comment? say what the current line has seen so far.
  (define (scan tokens)
    (let lp ([ts tokens] [line0 0] [char0 0] [opens '()] [block #f] [run #f]
             [code? #f] [comment? #f] [acc '()])
      (define (comment-run end acc)
        (if (and run (fx> end run))
            (cons (<fold> make
                    [start-line0 run]
                    [start-char0 #f]
                    [end-line0 end]
                    [end-char0 #f]
                    [kind "comment"])
              acc)
            acc))
      (match ts
        [()
         ;; The text may end without a newline after the run.
         (comment-run (if (and comment? (not code?)) line0 (fx- line0 1)) acc)]
        [(,t . ,rest)
         (let* ([type (token-type t)]
                [raw (token-raw t)]
                [next-line0 (fx+ line0 (count-newlines raw))]
                [next-char0 (char0-after char0 raw)])
           ;; Code ends a run of comment lines at the line before.
           (define (next opens block acc)
             (lp rest next-line0 next-char0 opens block
               #f #t comment? (comment-run (fx- line0 1) acc)))
           (cond
            [(eq? type 'eol)
             (if (and comment? (not code?))
                 (lp rest (fx+ line0 1) 0 opens block (or run line0) #f #f acc)
                 (lp rest (fx+ line0 1) 0 opens block #f #f #f
                   (comment-run (fx- line0 1) acc)))]
            [(eq? type 'ws)
             (lp rest line0 next-char0 opens block run code? comment? acc)]
            [(eq? type 'line-comment)
             (lp rest line0 next-char0 opens block run code? #t acc)]
            [(eq? type 'lblock-comment)
             (next opens
               (match block
                 [#f (list line0 next-char0 1)]
                 [(,start-line0 ,start-char0 ,depth)
                  (list start-line0 start-char0 (fx+ depth 1))])
               acc)]
            [(eq? type 'rblock-comment)
             (match block
               [#f (next opens block acc)]
               [(,start-line0 ,start-char0 1)
                (next opens #f
                  (cons (<fold> make
                          [start-line0 start-line0]
                          [start-char0 start-char0]
                          [end-line0 line0]
                          [end-char0 char0]
                          [kind "comment"])
                    acc))]
               [(,start-line0 ,start-char0 ,depth)
                (next opens (list start-line0 start-char0 (fx- depth 1)) acc)])]
            [block (next opens block acc)]
            [(token-open? t)
             (next
              (cons (list line0 next-char0
                      (and (eq? type 'lparen)
                           (import-next? rest)
                           "imports"))
                opens)
              block acc)]
            [(token-close? t)
             (match opens
               ;; An extra close paren has nothing to fold.
               [() (next opens block acc)]
               [((,start-line0 ,start-char0 ,kind) . ,opens)
                (next opens block
                  (cons (<fold> make
                          [start-line0 start-line0]
                          [start-char0 start-char0]
                          [end-line0 line0]
                          [end-char0 char0]
                          [kind kind])
                    acc))])]
            [(has-prop? t 'string)
             ;; Inside the quotes.
             (next opens block
               (cons (<fold> make
                       [start-line0 line0]
                       [start-char0 (fx+ char0 1)]
                       [end-line0 next-line0]
                       [end-char0 (fx- next-char0 1)]
                       [kind #f])
                 acc))]
            [else (next opens block acc)]))])))

  ;; Whether position 1 is before position 2, where a char of #f is the
  ;; end of the line.
  (define (pos<? line0-1 char0-1 line0-2 char0-2)
    (or (fx< line0-1 line0-2)
        (and (fx= line0-1 line0-2)
             char0-1
             (or (not char0-2) (fx< char0-1 char0-2)))))

  (define (normalize ranges limit line-only?)
    (define (char0 c) (if line-only? 0 c))
    (define (start<? a b)
      (<fold> open a a. [start-line0 start-char0])
      (<fold> open b b. [start-line0 start-char0])
      (pos<? a.start-line0 (char0 a.start-char0)
        b.start-line0 (char0 b.start-char0)))
    (define (end<? a b)
      (<fold> open a a. [end-line0 end-char0])
      (<fold> open b b. [end-line0 end-char0])
      (pos<? a.end-line0 (char0 a.end-char0) b.end-line0 (char0 b.end-char0)))
    (define (end-before-start? a b)
      (<fold> open a a. [end-line0 end-char0])
      (<fold> open b b. [start-line0 start-char0])
      (pos<? a.end-line0 (char0 a.end-char0)
        b.start-line0 (char0 b.start-char0)))
    (let* ([ranges
            (filter
             (lambda (r) (fx> (<fold> end-line0 r) (<fold> start-line0 r)))
             ranges)]
           ;; By start, and the outermost first among those that start
           ;; together.
           [ranges
            (sort
             (lambda (a b)
               (or (start<? a b)
                   (and (not (start<? b a)) (end<? b a))))
             ranges)]
           [nested (nest ranges line-only? end<? end-before-start?)])
      (map car
        (if (and limit (fx> (length nested) limit))
            (sort
             (lambda (a b) (start<? (car a) (car b)))
             (list-head
              ;; A stable sort keeps the order by start within a depth.
              (list-sort (lambda (a b) (fx< (cdr a) (cdr b))) nested)
              limit))
            nested))))

  ;; Given ranges sorted as normalize sorts them, returns the ranges that
  ;; nest properly, each paired with its depth. enclosing is the stack
  ;; of kept ranges that may enclose the next one. With line-only?, only
  ;; the first range that starts on a line is kept.
  (define (nest ranges line-only? end<? end-before-start?)
    (let lp ([ranges ranges] [enclosing '()] [last-start #f])
      (match ranges
        [() '()]
        [(,r . ,rest)
         (let ([enclosing
                (let pop ([ls enclosing])
                  (if (and (pair? ls) (end-before-start? (car ls) r))
                      (pop (cdr ls))
                      ls))])
           (cond
            [(and line-only? (eqv? (<fold> start-line0 r) last-start))
             (lp rest enclosing last-start)]
            [(and (pair? enclosing) (end<? (car enclosing) r))
             ;; r starts inside the enclosing range but ends after it.
             (lp rest enclosing last-start)]
            [else
             (cons (cons r (length enclosing))
               (lp rest (cons r enclosing) (<fold> start-line0 r)))]))])))
  )
