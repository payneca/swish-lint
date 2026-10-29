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

#!chezscheme
(library (indent)
  (export
   find-indent-start
   find-start-line1
   fold-indent
   has-prop?
   indent
   indent-tokens
   token
   token-bfp
   token-efp
   token-err
   token-length
   token-props
   token-raw
   token-type
   token-value
   tokenize
   )
  (import
   (chezscheme)
   (read)
   (swish imports)
   )
  (define $yield
    (make-process-parameter
     (lambda args (errorf 'yield "invalid context"))))

  (define (yield x) (($yield) x))

  (define-syntax generator
    (syntax-rules ()
      [(_ b1 b2 ...)
       (let ()
         (define (wrap return)
           (lambda (x)
             (set! return (call/1cc
                           (lambda (resume-here)
                             (set! proc resume-here)
                             (return x))))))
         (define (proc return)
           (parameterize ([$yield (wrap return)])
             b1 b2 ...))
         (lambda () (call/cc proc)))]))

  (define-record-type token
    (nongenerative)
    (fields
     (immutable type)
     (immutable type-index)
     (immutable value)
     (immutable raw)
     (immutable bfp)
     (immutable efp)
     (immutable err)
     (immutable props)
     ))

  (define-syntax define-token-predicates
    (syntax-rules ()
      [(_ indexer (pred? token ...) ...)
       (begin
         (define-enumeration Tokens (token ... ...) token-types)
         (define indexer (enum-set-indexer (token-types)))
         (define pred?
           (let ([set (#%$enum-set-members (token-types token ...))])
             (lambda (t)
               (cond
                [(token-type-index t) =>
                 (lambda (hit)
                   (#3%fxbit-set? set hit))]
                [else #f]))))
         ...
         )]))

  (define-token-predicates token-type-indexer
    (token-open? lparen lbrack record-brack vflnparen vflparen vfxnparen vfxparen vnparen vparen vu8nparen vu8paren vsnparen vsparen)
    (token-close? rparen rbrack)
    (no-indent? lparen lbrack record-brack vflnparen vflparen vfxnparen vfxparen vnparen vparen vu8nparen vu8paren vsnparen vsparen rparen rbrack quote box)
    (simple-open? lparen lbrack)
    (simple-close? rparen rbrack)
    (atomic-or-quote? atomic quote))

  (define-enumeration prop-element
    (
     block-comment
     char
     comment
     datum-comment
     leading-whitespace
     left-margin-whitespace
     line-comment
     number
     right-column-whitespace
     string
     trailing-whitespace
     with-code-whitespace
     )
    token-prop)

  (define empty-props (token-prop))

  (define delims
    '("[^\\S\\n]+"               ; contiguous whitespace, not newlines
      "\\s"                      ; other whitespace cases
      ";+[^\\n]*"                ; line comments
      "#\\|"                     ; lblock-comment
      "\\|#"                     ; rblock-comment
      ))

  (define my-regexp
    (re (join delims #\|)))

  (define (open-token-string text start-line end-line)
    ;; tokens should be in the inclusive range [start-line, end-line],
    ;; meaning that a multi-line string that crosses a boundary should
    ;; be included in the output. start-line and end-line are 1-based.
    ;;
    ;; We always tokenize from the beginning of the text, because a
    ;; line may begin inside a multi-line string or block comment.
    ;; line-number is the line at the start of the next token.
    (define line-number 1)
    (define (count-newlines bfp efp)
      ;; count using the original text to avoid allocation
      (do ([i bfp (fx+ i 1)]
           [c 0 (if (eq? (string-ref text i) #\newline) (fx+ c 1) c)])
          ((fx= i efp) c)))
    (define (in-range? bfp efp)
      ;; Every token is built in order, so this advances line-number
      ;; past the token whether or not it is in range. An eol token
      ;; belongs to the line it ends.
      (let* ([first line-number]
             [n (count-newlines bfp efp)]
             [last (if (and (fx> n 0)
                            (eq? (string-ref text (fx- efp 1)) #\newline))
                       (fx+ first (fx- n 1))
                       (fx+ first n))])
        (set! line-number (fx+ first n))
        (and (<= first end-line) (<= start-line last))))
    (define (yield+ t)
      (when t
        (yield t)))
    (define (build-token* type value bfp efp err props)
      (and (in-range? bfp efp)
           (make-token type (token-type-indexer type) value value bfp efp err props)))
    (define (build-token type value bfp efp err props)
      (and (in-range? bfp efp)
           (make-token type (token-type-indexer type) value (substring text bfp efp) bfp efp err props)))
    (define (as-token str start end err)
      (and
       (fx< start end)
       (let ([first (string-ref str start)]
             [at-least-two? (> (- end start) 1)])
         (cond
          [(eq? first #\newline)
           (build-token 'eol #f start end err empty-props)]
          [(char-whitespace? first)
           (build-token 'ws (- end start) start end err empty-props)]
          [(eq? first #\;)
           (build-token* 'line-comment (substring str start end) start end err (token-prop comment line-comment))]
          [(and at-least-two?
                (eq? first #\#)
                (eq? (string-ref str (fx+ start 1)) #\|))
           (build-token 'lblock-comment #f start end err empty-props)]
          [(and at-least-two?
                (eq? first #\|)
                (eq? (string-ref str (fx+ start 1)) #\#))
           (build-token 'rblock-comment #f start end err empty-props)]
          [else
           (build-token* 'text (substring str start end) start end err empty-props)]))))
    (define (gen-ext-tokens start end err)
      (when (and (fx< start end)
                 (<= line-number end-line))
        (let ([c (string-ref text start)])
          (cond
           [(eq? c #\newline)           ; newline
            (let ([next (fx+ start 1)])
              (yield+ (build-token 'eol #f start next err empty-props))
              (gen-ext-tokens next end err))]
           [(eq? c #\;)                 ; line comments
            (let lp ([next (fx+ start 1)])
              (cond
               [(or (fx>= next end)
                    (eq? (string-ref text next) #\newline))
                (yield+
                 (build-token* 'line-comment
                   (substring text start next)
                   start
                   next
                   err
                   (token-prop comment line-comment)))
                (gen-ext-tokens next end err)]
               [else
                (lp (fx+ next 1))]))]
           [(and (eq? c #\#)            ; lblock-comment
                 (let ([next (fx+ start 1)])
                   (and (fx< next end)
                        (eq? (string-ref text next) #\|)
                        (fx+ next 1)))) =>
            (lambda (next)
              (yield+ (build-token 'lblock-comment #f start next err empty-props))
              (gen-block-comment-tokens next end err 1))]
           [(and (eq? c #\|)            ; rblock-comment
                 (let ([next (fx+ start 1)])
                   (and (fx< next end)
                        (eq? (string-ref text next) #\#)
                        (fx+ next 1)))) =>
            (lambda (next)
              (yield+ (build-token 'rblock-comment #f start next err empty-props))
              (gen-ext-tokens next end err))]
           [(char-whitespace? c) ; contiguous whitespace, not newlines
            (let lp ([next (fx+ start 1)])
              (cond
               [(or (fx>= next end)
                    (let ([c2 (string-ref text next)])
                      (or (eq? c2 #\newline)
                          (not (char-whitespace? c2)))))
                (yield+ (build-token 'ws (fx- next start) start next err empty-props))
                (gen-ext-tokens next end err)]
               [else
                (lp (fx+ next 1))]))]
           [else
            (match (pregexp-match-positions my-regexp text start end)
              [((,bfp . ,efp))
               ;; Something occurred before the regexp
               (cond [(as-token text start bfp err) => yield+])
               ;; Regexp found something
               (cond [(as-token text bfp efp err) => yield+])
               (gen-ext-tokens efp end err)]
              [,_
               ;; Something occurred that was not processed by read-token,
               ;; and was not caught by the regular expressions.
               (cond [(as-token text start end err) => yield+])])]))))
    (define (gen-block-comment-tokens start end err depth)
      ;; Inside a block comment, only #| and |# matter. In particular,
      ;; a ; doesn't start a line comment that would hide the |#.
      (define (block-delim? i)
        (and (fx< (fx+ i 1) end)
             (let ([c (string-ref text i)]
                   [c2 (string-ref text (fx+ i 1))])
               (or (and (eq? c #\#) (eq? c2 #\|))
                   (and (eq? c #\|) (eq? c2 #\#))))))
      (cond
       [(fx= depth 0) (gen-ext-tokens start end err)]
       [(and (fx< start end)
             (<= line-number end-line))
        (let ([c (string-ref text start)])
          (cond
           [(eq? c #\newline)
            (let ([next (fx+ start 1)])
              (yield+ (build-token 'eol #f start next err empty-props))
              (gen-block-comment-tokens next end err depth))]
           [(block-delim? start)
            (let ([next (fx+ start 2)])
              (cond
               [(eq? c #\#)
                (yield+ (build-token 'lblock-comment #f start next err empty-props))
                (gen-block-comment-tokens next end err (fx+ depth 1))]
               [else
                (yield+ (build-token 'rblock-comment #f start next err empty-props))
                (gen-block-comment-tokens next end err (fx- depth 1))]))]
           [(char-whitespace? c) ; contiguous whitespace, not newlines
            (let lp ([next (fx+ start 1)])
              (cond
               [(or (fx>= next end)
                    (let ([c2 (string-ref text next)])
                      (or (eq? c2 #\newline)
                          (not (char-whitespace? c2)))))
                (yield+ (build-token 'ws (fx- next start) start next err empty-props))
                (gen-block-comment-tokens next end err depth)]
               [else
                (lp (fx+ next 1))]))]
           [else
            (let lp ([next (fx+ start 1)])
              (cond
               [(or (fx>= next end)
                    (char-whitespace? (string-ref text next))
                    (block-delim? next))
                (yield+ (build-token* 'text (substring text start next) start next err empty-props))
                (gen-block-comment-tokens next end err depth)]
               [else
                (lp (fx+ next 1))]))]))]))
    (define (gen-tokens)
      (define end (string-length text))
      ;; When read-token fails on a string, as on an escape it rejects,
      ;; find the string by its quotes alone: from its opening quote to
      ;; the next quote that a backslash doesn't escape. Returns the
      ;; string's bfp and efp, or #f #f. The string may follow
      ;; whitespace and comments, which read-token skips.
      (define (failed-string start)
        (define (char-at i) (and (fx< i end) (string-ref text i)))
        (define (skip-line i)
          (if (or (fx>= i end) (eqv? (char-at i) #\newline))
              i
              (skip-line (fx+ i 1))))
        (define (skip-block i depth)
          (cond
           [(fx>= i end) #f]
           [(and (eqv? (char-at i) #\|) (eqv? (char-at (fx+ i 1)) #\#))
            (if (fx= depth 1)
                (fx+ i 2)
                (skip-block (fx+ i 2) (fx- depth 1)))]
           [(and (eqv? (char-at i) #\#) (eqv? (char-at (fx+ i 1)) #\|))
            (skip-block (fx+ i 2) (fx+ depth 1))]
           [else (skip-block (fx+ i 1) depth)]))
        (define (closing-quote i)
          (match (char-at i)
            [#f #f]
            [#\\ (closing-quote (fx+ i 2))]
            [#\" (fx+ i 1)]
            [,_ (closing-quote (fx+ i 1))]))
        (let skip ([i start])
          (let ([c (char-at i)])
            (cond
             [(not c) (values #f #f)]
             [(char-whitespace? c) (skip (fx+ i 1))]
             [(eqv? c #\;) (skip (skip-line i))]
             [(and (eqv? c #\#) (eqv? (char-at (fx+ i 1)) #\|))
              (let ([j (skip-block (fx+ i 2) 1)])
                (if j (skip j) (values #f #f)))]
             [(eqv? c #\")
              (let ([j (closing-quote (fx+ i 1))])
                (if j (values i j) (values #f #f)))]
             [else (values #f #f)]))))
      (let ([ip (open-input-string text)])
        (when (and (fx> end 2)
                   (eq? (string-ref text 0) #\#)
                   (eq? (string-ref text 1) #\!)
                   (let ([x (string-ref text 2)])
                     (or (eq? x #\space)
                         (eq? x #\/))))
          ;; Advance the port, attempt to parse the entire line as
          ;; extended tokens and continue.
          (get-line ip)
          (gen-ext-tokens 0 (port-position ip) #f))
        (let lp ([prior-efp (port-position ip)])
          (when (and (< prior-efp end)
                     (<= line-number end-line))
            (match (try (let-values ([(type value bfp efp) (read-token ip)])
                          (unless (= prior-efp bfp)
                            ;; Something occurred before the read token, read extended tokens first.
                            (gen-ext-tokens prior-efp bfp #f))
                          (unless (= bfp efp)
                            (yield+ (build-token type value bfp efp #f
                                      (cond
                                       [(eq? type 'atomic)
                                        (cond
                                         [(string? value) (token-prop string)]
                                         [(number? value) (token-prop number)]
                                         [(char? value) (token-prop char)]
                                         [else empty-props])]
                                       [else
                                        empty-props]))))
                          efp))
              [`(catch ,r ,err)
               (let ([s (exit-reason->english r)])
                 (cond
                  [(starts-with? s "Exception in read-token: unexpected end-of-file")
                   ;; Rewind the port, attempt to parse the entire
                   ;; line as extended tokens and continue.
                   (set-port-position! ip prior-efp)
                   (get-line ip)
                   (let ([efp (port-position ip)])
                     (gen-ext-tokens prior-efp efp err)
                     (lp efp))]
                  [else
                   (let-values ([(bfp efp) (failed-string prior-efp)])
                     (cond
                      [bfp
                       ;; Keep the string whole, so the code after it
                       ;; stays code, and continue after it.
                       (gen-ext-tokens prior-efp bfp #f)
                       (yield+ (build-token 'atomic (substring text bfp efp)
                                 bfp efp err (token-prop string)))
                       (set-port-position! ip efp)
                       (lp efp)]
                      [else
                       ;; Advance the port to the next whitespace
                       ;; character, attempt to parse the failed
                       ;; characters as extended tokens and continue.
                       (let lp ()
                         (let ([c (get-char ip)])
                           (if (or (eof-object? c) (char-whitespace? c))
                               (unget-char ip c)
                               (lp))))
                       (let ([efp (port-position ip)])
                         (gen-ext-tokens prior-efp efp err)
                         (lp efp))]))]))]
              [,efp (lp efp)])))))
    (arg-check 'open-token-string
      [text string?]
      [start-line fixnum? (lambda (x) (fx> x 0))]
      [end-line fixnum? (lambda (x) (fx>= x start-line))])
    (generator
     (gen-tokens)
     (yield (eof-object))))

  (define (read-extended-token tg)
    (tg))

  (define (token-length t)
    (string-length (token-raw t)))

  (define (parse text start-line end-line)
    (let ([tp (open-token-string text start-line end-line)])
      (let lp ()
        (let ([t (read-extended-token tp)])
          (if (eof-object? t)
              '()
              (cons t (lp)))))))

  (define (make-token-port ls)
    (let ([ls ls])
      (lambda (op)
        (match op
          [get-token
           (match ls
             [() (eof-object)]
             [(,x . ,rest)
              (set! ls rest)
              x])]
          [peek-token
           (match ls
             [() (eof-object)]
             [(,x . ,_) x])]
          [make-shallow-copy
           (make-token-port ls)]))))

  (define (get-token tp)
    (tp 'get-token))

  (define (peek-token tp)
    (tp 'peek-token))

  (define (copy-token-input-port tp)
    (tp 'make-shallow-copy))

  (define (make-token-output-port)
    (let ([ls '()]
          [column 0])
      (lambda (op)
        (match op
          [#(put ,t)
           (set! ls (cons t ls))
           (set! column
             (if (eq? (token-type t) 'eol)
                 0
                 (+ column (token-length t))))]
          [get-column column]
          [get-all
           (let ([result (reverse ls)])
             (set! ls '())
             (set! column 0)
             result)]))))

  (define (put-token tp x)
    (tp `#(put ,x)))

  (define (get-output-tokens tp)
    (tp 'get-all))

  (define (get-column tp)
    (tp 'get-column))

  (define (add-prop* props prop)
    (enum-set-union props prop))

  (define (add-prop ls prop)
    (map
     (lambda (t)
       (make-token
        (token-type t)
        (token-type-index t)
        (token-value t)
        (token-raw t)
        (token-bfp t)
        (token-efp t)
        (token-err t)
        (add-prop* (token-props t) prop)))
     ls))

  (define (has-prop? t prop)
    (enum-set-member? prop (token-props t)))

  (define-syntax token-cond
    (syntax-rules (eof)
      [(_ $t $type [eof $eof-expr ...] clauses ...)
       (and (identifier? #'$t) (identifier? #'$type))
       (cond
        [(eof-object? $t) $eof-expr ...]
        [else
         (let ([$type (token-type $t)])
           (cond clauses ...))])]))

  (define (tokens-until* tp keep? pred)
    (let lp ([acc '()])
      (let ([t (peek-token tp)])
        (token-cond t type
          [eof (reverse acc)]
          [(pred type)
           (reverse
            (if keep?
                (cons (get-token tp) acc)
                acc))]
          [else
           (lp (cons (get-token tp) acc))]))))

  (define (tokens-until tp ttype)
    (tokens-until* tp #t
      (lambda (type) (equal? type ttype))))

  (define (line-tokens tp)
    (tokens-until tp 'eol))

  (define (space-tokens tp)
    (tokens-until* tp #f
      (lambda (type)
        (not (eq? type 'ws)))))

  (define (block-tokens tp)
    (let lp ([depth 1] [acc '()])
      (cond
       [(= depth 0) (reverse acc)]
       [else
        (let ([t (get-token tp)])
          (token-cond t type
            [eof (reverse acc)]
            [(eq? type 'lblock-comment)
             (lp (+ depth 1) (cons t acc))]
            [(eq? type 'rblock-comment)
             (lp (- depth 1) (cons t acc))]
            [else
             (lp depth (cons t acc))]))])))

  (define (expr-tokens tp)
    (let lp ([armed? #f] [depth 0] [acc '()])
      (cond
       [(and armed? (= depth 0)) (reverse acc)]
       [else
        (let ([t (get-token tp)])
          (token-cond t type
            [eof (reverse acc)]
            [(eq? type 'line-comment)
             (let ([comment (line-tokens tp)])
               (lp armed? depth
                 (fold-right cons (cons t acc) (reverse comment))))]
            [(eq? type 'lblock-comment)
             ;; As the reader does, skip a block comment; it isn't the
             ;; datum.
             (let ([comment (block-tokens tp)])
               (lp armed? depth
                 (fold-right cons (cons t acc) (reverse comment))))]
            [(and (eq? type 'quote) (eq? (token-value t) 'datum-comment))
             ;; A nested #; and the datum it comments out aren't the
             ;; datum either.
             (let ([skipped (expr-tokens tp)])
               (lp armed? depth
                 (fold-right cons (cons t acc) (reverse skipped))))]
            [(token-open? t)
             (lp #t (+ depth 1) (cons t acc))]
            [(token-close? t)
             (lp armed? (- depth 1) (cons t acc))]
            [(memq type '(quote box mark))
             ;; A prefix, such as ' or #& or #0=, belongs to the datum
             ;; that follows it.
             (lp armed? depth (cons t acc))]
            [(or (eq? type 'eol)
                 (eq? type 'ws))
             (lp armed? depth (cons t acc))]
            [else
             (lp #t depth (cons t acc))]))])))

  (define (mark tp)
    (let ([t (get-token tp)])
      (token-cond t type
        [eof '()]
        [(and (eq? type 'quote) (eq? (token-value t) 'datum-comment))
         (let* ([tokens (mark (make-token-port (expr-tokens tp)))]
                [expr (cons t tokens)])
           (append
            (add-prop expr (token-prop comment datum-comment))
            (mark tp)))]
        [(eq? type 'lblock-comment)
         (let ([comment (cons t (block-tokens tp))])
           (append
            (add-prop comment (token-prop comment block-comment))
            (mark tp)))]
        [else
         (cons t (mark tp))])))

  (define (count-leading-semi s limit)
    (let ([s-len (string-length s)])
      (let lp ([n 0])
        (cond
         [(and (fx< n limit)
               (fx< n s-len)
               (eq? (string-ref s n) #\;))
          (lp (fx+ n 1))]
         [else n]))))

  (define (mark-whitespace tp)
    (let lp ([start-of-line? #t])
      (let ([t (get-token tp)])
        (token-cond t type
          [eof '()]
          [(eq? type 'eol)
           (cons t (lp #t))]
          [(eq? type 'ws)
           (let* ([spaces (cons t (space-tokens tp))]
                  [next (peek-token tp)]
                  [spaces
                   (token-cond next type
                     [eof spaces]
                     [(eq? type 'eol)
                      (add-prop spaces (token-prop trailing-whitespace))]
                     [(eq? type 'line-comment)
                      (match (count-leading-semi (token-raw next) 3)
                        [3 (add-prop spaces (token-prop left-margin-whitespace))]
                        [2 (add-prop spaces (token-prop with-code-whitespace))]
                        [1 (add-prop spaces (token-prop right-column-whitespace))])]
                     [start-of-line?
                      (add-prop spaces (token-prop leading-whitespace))]
                     [else spaces])])
             (append spaces (lp start-of-line?)))]
          [else
           (cons t (lp #f))]))))

  (define (scheme-no-indent t aol? indent)
    0)

  (define (scheme-normal-indent t aol? indent)
    (if aol?
        (+ (token-length t) 1)
        0))

  (define (scheme-nice-indent t aol? indent)
    (if aol? 1 0))

  (define (scheme-special-indent count t aol? indent)
    (if (and aol? (<= count 0))
        (+ (token-length t) 1)
        1))

  (define-syntax define-indent-table
    (syntax-rules ()
      [(_ name [ids n] ...)
       (define name
         (let ([ht (make-eq-hashtable)])
           (for-each (lambda (id) (eq-hashtable-set! ht id n)) 'ids)
           ...
           ht))]))

  (define-indent-table indent-table
    [(and
      if
      or
      = < > <= >= + - * /
      fx= fx< fx> fx<= fx>= fx+ fx- fx* fx/
      fl= fl< fl> fl<= fl>= fl+ fl- fl* fl/
      cfl= cfl+ cfl- cfl* cfl/
      quotient remainder modulo max min
      fxquotient fxremainder fxmodulo fxmax fxmin
      logand logior logor logxor
      fxlogand fxlogior fxlogor fxlogxor)
     scheme-normal-indent]
    [(do syntax-case) 2]
    [(begin delay dynamic-wind) 0]
    ;; Manual updates to style
    [(! @ $ % ^ & : ? ~ _) scheme-normal-indent]
    )

  (define (emit-whitespace op n)
    (when (> n 0)
      (let ([raw (make-string n #\space)])
        (put-token op
          (make-token 'ws (token-type-indexer 'ws) raw raw #f #f #f empty-props)))))

  (define (emit-tokens op ls)
    (match ls
      [() (void)]
      [(,first . ,rest)
       (put-token op first)
       (emit-tokens op rest)]))

  (define prefixed-name-regexp (pregexp (format "^~a(.+)" primitive-prefix)))

  ;; A primitive reference such as #%fx+ indents like the name itself.
  (define (lookup-indent name)
    (let ([name (match (pregexp-match prefixed-name-regexp name)
                  [(,_ ,name) name]
                  [#f name])])
      (eq-hashtable-ref indent-table (string->symbol name) #f)))

  ;; at-file-start? says whether the tokens start at the top of the file.
  ;; If so, a top-level form goes to column 0. If not, a top-level line
  ;; keeps its indentation, since it may be inside a form that starts
  ;; above the tokens, and the lines below are indented relative to it.
  (define (indent-code tokens at-file-start?)
    (let* ([tokens (mark-whitespace (make-token-port tokens))]
           [ip (make-token-port tokens)]
           [op (make-token-output-port)])
      (define-tuple <level> indent lin subform-count)

      (define default-lin -1)

      (define (subform-indent t aol? indent)
        (token-cond t type
          [eof (values 0 default-lin)]
          ;; Because tokens are treated differently than in other
          ;; environments, we need to make sure they provide no
          ;; additional indentation during subform-indent.
          [(no-indent? t)
           (values 0 default-lin)]
          [(or (has-prop? t 'number)
               (has-prop? t 'string)
               (has-prop? t 'char))
           (values 0 -2)]
          [else
           (let ([x (lookup-indent (token-raw t))])
             (cond
              [(not x)
               (values (scheme-nice-indent t aol? indent) default-lin)]
              [(integer? x)
               (values (scheme-special-indent x t aol? indent) x)]
              [else
               (values (x t aol? indent) default-lin)]))]))

      (define (increment-subform-count ls)
        (match ls
          [() ls]
          [(,first . ,rest)
           (cons (<level> copy* first [subform-count (+ subform-count 1)])
             rest)]))

      (define (start-new-line rlevels)
        (match rlevels
          [() (s0 0 rlevels)]
          [(`(<level> ,indent) . ,_)
           (s0 indent rlevels)]))

      (define (s0 indent rlevels)
        ;; start of line; emit leading whitespace
        (let ([t (get-token ip)])
          (token-cond t type
            [eof (void)]
            [(eq? type 'eol)
             (put-token op t)
             (start-new-line rlevels)]
            [(eq? type 'ws)
             (s0
              (if (or (has-prop? t 'with-code-whitespace)
                      (and at-file-start?
                           (null? rlevels)
                           (has-prop? t 'leading-whitespace)))
                  indent
                  (+ indent (token-length t)))
              rlevels)]
            [else
             (let ([indent
                    (match rlevels
                      [() indent]
                      [(`(<level> ,indent ,lin ,subform-count) . ,_)
                       (if (< subform-count lin)
                           (+ indent 2)
                           indent)])])
               (emit-whitespace op indent)
               (s1 t indent rlevels))])))

      (define (s1 t indent rlevels)
        ;; line cont'd
        (token-cond t type
          [eof (void)]
          [(eq? type 'eol)
           (put-token op t)
           (start-new-line rlevels)]
          [(has-prop? t 'line-comment)
           (put-token op t)
           (s1 (get-token ip) indent rlevels)]
          [(eq? type 'lblock-comment)
           (put-token op t)
           (emit-tokens op (block-tokens ip))
           (s1 (get-token ip) indent rlevels)]
          [(token-open? t)
           (put-token op t)
           (let* ([indent (+ indent (token-length t))]
                  [next (get-token ip)]
                  [aol? (atom-on-line? next ip)])
             (let-values ([(delta lin) (subform-indent next aol? indent)])
               (s1 next indent
                 (cons (<level> make
                         [indent (+ indent delta)]
                         [lin lin]
                         [subform-count -1])
                   rlevels))))]
          [(token-close? t)
           (put-token op t)
           ;; When the parens are unbalanced, this code continues on
           ;; allowing upcoming subforms to indent according to the
           ;; current form's position.
           (s1 (get-token ip) (+ indent 1)
             (increment-subform-count
              (match rlevels
                [() '()]                ; unbalanced parens
                [(,_ . ,rest) rest])))]
          [(eq? type 'ws)
           (put-token op t)
           (s1 (get-token ip) (+ indent (token-length t)) rlevels)]
          [else
           (put-token op t)
           (s1 (get-token ip)
             (+ indent (token-length t))
             (increment-subform-count rlevels))]))

      (define (atom-on-line? first ip)
        (let ([ip (copy-token-input-port ip)])
          (define (s0 t)
            (token-cond t type
              [eof #f]
              [(eq? type 'eol) #f]
              [(simple-open? t)
               (s1 (get-token ip))]
              [(and (atomic-or-quote? t)
                    (not (has-prop? t 'line-comment))
                    (not (has-prop? t 'block-comment)))
               #t]
              [else (s0 (get-token ip))]))
          (define (s1 t)
            (token-cond t type
              [eof #f]
              [(eq? type 'eol) #f]
              [(eq? type 'ws) (s1 (get-token ip))]
              [(simple-close? t) #t]
              [(and (atomic-or-quote? t)
                    (not (has-prop? t 'line-comment))
                    (not (has-prop? t 'block-comment)))
               #t]
              [else (s1 (get-token ip))]))
          (token-cond first type
            [eof #f]
            [(eq? type 'eol) #f]
            [else (s0 (get-token ip))])))

      (start-new-line '())
      (get-output-tokens op)))

  (define middle-anchor 40)
  (define max-fill-col 70)            ; Default fill-column from Emacs

  (define-tuple <marker>
    anchor-min
    anchor-max
    )

  (define (build-comment-markers tokens)
    (define ip (make-token-port tokens))
    (define ht (make-eq-hashtable))
    (let lp ([line 1] [code-end 0] [line-comment? #f] [line-comment-length 0])
      (let ([t (get-token ip)])
        (token-cond t type
          [eof ht]
          [(eq? type 'eol)
           (when line-comment?
             (eq-hashtable-set! ht line
               (<marker> make
                 [anchor-min (+ code-end 1)]
                 [anchor-max (max (+ code-end 1)
                                  (- max-fill-col line-comment-length))])))
           (lp (+ line 1) 0 #f 0)]
          [(has-prop? t 'line-comment)
           (lp line code-end #t (+ (token-length t) line-comment-length))]
          [(and (eq? type 'ws)
                (has-prop? t 'right-column-whitespace))
           (lp line code-end line-comment? line-comment-length)]
          [else
           (lp line (+ code-end (token-length t)) line-comment? line-comment-length)]))))

  (define (build-anchors markers)
    (define ht (make-eq-hashtable))
    (define lines (vector->list (vector-sort < (hashtable-keys markers))))

    (define (minmax marker)
      (match marker
        [#f (values 0 max-fill-col)]
        [`(<marker> ,anchor-min ,anchor-max)
         (values anchor-min anchor-max)]))

    (define (look-ahead line curr-min curr-max)
      (match (eq-hashtable-ref markers line #f)
        [#f (values curr-min curr-max)]
        [`(<marker> ,anchor-min ,anchor-max)
         (if (and (<= curr-min anchor-max) (>= curr-max anchor-min))
             (look-ahead (+ line 1)
               (max curr-min anchor-min)
               (min curr-max anchor-max))
             (values curr-min curr-max))]))

    (let lp ([lines lines]
             [prev-line (most-negative-fixnum)]
             [prev-anchor middle-anchor])
      (match lines
        [() ht]
        [(,line . ,rest)
         (let*-values ([(curr-min curr-max)
                        (minmax (eq-hashtable-ref markers line #f))]
                       [(curr-min curr-max)
                        (look-ahead (+ line 1) curr-min curr-max)])
           (let* ([anchor
                   (cond
                    [(and (= line (+ prev-line 1))
                          (< prev-anchor max-fill-col)
                          (<= curr-min prev-anchor curr-max))
                     prev-anchor]
                    [else middle-anchor])]
                  [anchor
                   (cond
                    [(<= curr-min anchor curr-max) anchor]
                    [(< anchor curr-min) curr-min]
                    [else curr-max])])
             (eq-hashtable-set! ht line anchor)
             (lp rest line anchor)))])))

  (define (indent-comments tokens)
    (let* ([tokens (mark-whitespace (make-token-port tokens))]
           [markers (build-comment-markers tokens)]
           [anchors (build-anchors markers)]
           [ip (make-token-port tokens)]
           [op (make-token-output-port)])
      (let lp ([line 1] [prior #f])
        (define (align-middle line t)
          (let* ([anchor (eq-hashtable-ref anchors line middle-anchor)]
                 [spaces (max (- anchor (get-column op)) 1)])
            (emit-whitespace op spaces)
            (put-token op t)))

        (let ([t (get-token ip)])
          (token-cond t type
            [eof (void)]
            [(eq? type 'eol)
             (put-token op t)
             (lp (+ line 1) #f)]
            [(has-prop? t 'block-comment)
             (put-token op t)
             (lp line t)]
            [(has-prop? t 'right-column-whitespace)
             ;; Don't actually write right-column whitespace yet.
             (lp line t)]
            [(= (count-leading-semi (token-raw t) 2) 1)
             (cond
              [(or (not prior)
                   (and prior (has-prop? prior 'right-column-whitespace)))
               (align-middle line t)
               (lp line t)]
              [else
               (put-token op t)
               (lp line t)])]
            [else
             (put-token op t)
             (lp line t)])))

      (get-output-tokens op)))

  (define tokenize
    (case-lambda
     [(text) (tokenize text 1 (most-positive-fixnum))]
     [(text start-line end-line)
      (let* ([tokens (parse text start-line end-line)]
             [tokens (mark (make-token-port tokens))])
        tokens)]))

  (define (indent-tokens tokens at-file-start?)
    (let* ([tokens (indent-code tokens at-file-start?)]
           [tokens (indent-comments tokens)])
      tokens))

  (define (display-tokens ls op)
    (for-each
     (lambda (t)
       (display (token-raw t) op))
     ls))

  (define (tokens->string ls)
    (let ([op (open-output-string)])
      (display-tokens ls op)
      (get-output-string op)))

  (define indent
    (case-lambda
     [(text)
      (indent text 1 (most-positive-fixnum))]
     [(text start-line1 end-line1)
      (tokens->string
       (indent-tokens (tokenize text start-line1 end-line1)
         (= start-line1 1)))]))

  ;; Unless at-file-start? is given, text is the whole file, so it's
  ;; true when indenting starts at line 1.
  (define fold-indent
    (case-lambda
     [(text start-line1 end-line1 init proc)
      (fold-indent text start-line1 end-line1 init proc (= start-line1 1))]
     [(text start-line1 end-line1 init proc at-file-start?)
      ($fold-indent text start-line1 end-line1 init proc at-file-start?)]))

  (define ($fold-indent text start-line1 end-line1 init proc at-file-start?)
    ;; [start-line1, end-line1] inclusive
    (define (fp->line1 fp)
      (do ([i 0 (fx+ i 1)]
           [n 1 (if (eq? (string-ref text i) #\newline) (fx+ n 1) n)])
          ((fx= i fp) n)))
    (let* ([tokens (tokenize text start-line1 end-line1)]
           ;; A token that crosses into the range, such as a
           ;; multi-line string, is included, so the indented text may
           ;; begin on an earlier line. Align the source with it, and
           ;; report only lines in the range.
           [first-line1
            (match tokens
              [() start-line1]
              [(,t . ,_) (min start-line1 (fp->line1 (token-bfp t)))])]
           [src-port (open-input-string text)]
           [dst-port (open-input-string
                      (tokens->string (indent-tokens tokens at-file-start?)))])
      (do ([i 1 (+ i 1)]) ((= i first-line1))
        (get-line src-port))
      (let lp ([line first-line1] [acc init])
        (if (or (eof-object? (peek-char src-port))
                (> line end-line1))
            acc
            (let ([src-line (get-line src-port)]
                  [dst-line (get-line dst-port)])
              (lp (+ line 1)
                (if (< line start-line1)
                    acc
                    (proc line src-line dst-line acc))))))))

  ;; Returns the 1-based line where indenting can start and still
  ;; produce the right indentation for line1, or #f when line1 starts
  ;; inside a string, block comment, or |symbol|. Starting at the line
  ;; of the innermost open paren that encloses line1 keeps that line's
  ;; existing indentation, so line1 is indented relative to the code
  ;; around it rather than to how the whole text would be indented.
  ;; This makes one pass over the text before line1 tracking only what
  ;; is needed to match parens.
  (define (find-start-line1 text line1)
    (let-values ([(line paren-fp) (find-indent-start text line1)])
      line))

  ;; Like find-start-line1, but also returns the position of the
  ;; enclosing paren when its line begins inside a string, comment, or
  ;; |symbol|. Indenting must then start at the paren rather than at the
  ;; start of the line. Otherwise the second value is #f.
  (define (find-indent-start text line1)
    (define end (string-length text))
    (define line 1)
    (define (char-at i) (and (< i end) (string-ref text i)))
    (define (newline!)
      ;; returns #t when line1 starts inside a string or comment
      (set! line (+ line 1))
      (= line line1))
    (define (skip-to-newline i)
      (if (or (>= i end) (eqv? (string-ref text i) #\newline))
          i
          (skip-to-newline (+ i 1))))
    (define (skip-delimited i delim)
      ;; returns the index after the closing delim, or #f
      (let ([c (char-at i)])
        (cond
         [(not c) #f]
         [(eqv? c delim) (+ i 1)]
         [(eqv? c #\\)
          (if (and (eqv? (char-at (+ i 1)) #\newline) (newline!))
              #f
              (skip-delimited (+ i 2) delim))]
         [(and (eqv? c #\newline) (newline!)) #f]
         [else (skip-delimited (+ i 1) delim)])))
    (define (skip-block-comment i depth)
      ;; returns the index after the matching |#, or #f
      (let ([c (char-at i)])
        (cond
         [(not c) #f]
         [(and (eqv? c #\|) (eqv? (char-at (+ i 1)) #\#))
          (if (= depth 1)
              (+ i 2)
              (skip-block-comment (+ i 2) (- depth 1)))]
         [(and (eqv? c #\#) (eqv? (char-at (+ i 1)) #\|))
          (skip-block-comment (+ i 2) (+ depth 1))]
         [(and (eqv? c #\newline) (newline!)) #f]
         [else (skip-block-comment (+ i 1) depth)])))
    ;; The loop stops at the newline that starts line1, which never
    ;; happens for the first line. It would scan the whole text and
    ;; could return a later line.
    (if (<= line1 1)
        (values line1 #f)
        ;; opens holds each open paren's line, and its position when
        ;; that line begins inside a string, comment, or |symbol|.
        ;; inside? says whether the current line begins inside one.
        (let lp ([i 0] [opens '()] [inside? #f])
          (define (result)
            (match opens
              [() (values line1 #f)]
              [((,open-line . ,fp) . ,_) (values open-line fp)]))
          ;; j is the index after a string, comment, or |symbol| that
          ;; began on line before. If it crossed a newline, the current
          ;; line begins inside it.
          (define (after j before)
            (if j
                (lp j opens (or inside? (not (= line before))))
                (values #f #f)))
          (let ([c (char-at i)])
            (case c
              [(#f) (result)]
              [(#\newline) (if (newline!) (result) (lp (+ i 1) opens #f))]
              [(#\;) (lp (skip-to-newline i) opens inside?)]
              [(#\" #\|)
               (let ([before line])
                 (after (skip-delimited (+ i 1) c) before))]
              [(#\#)
               (case (char-at (+ i 1))
                 [(#\\)
                  ;; Skip the character so #\( is not a paren, but let
                  ;; the loop see #\ followed by a newline.
                  (if (eqv? (char-at (+ i 2)) #\newline)
                      (lp (+ i 2) opens inside?)
                      (lp (+ i 3) opens inside?))]
                 [(#\|)
                  (let ([before line])
                    (after (skip-block-comment (+ i 2) 1) before))]
                 ;; The datum after #; is still code.
                 [(#\;) (lp (+ i 2) opens inside?)]
                 [else (lp (+ i 1) opens inside?)])]
              [(#\( #\[)
               (lp (+ i 1) (cons (cons line (and inside? i)) opens) inside?)]
              [(#\) #\])
               (lp (+ i 1) (if (null? opens) opens (cdr opens)) inside?)]
              [else (lp (+ i 1) opens inside?)])))))

  (record-writer (csv7:record-type-descriptor (token-prop))
    (lambda (r p wr)
      (fprintf p "#<token-prop {~{~a~^,~}}>" (enum-set->list r))))
  )
