;;; Copyright 2025 Chris Payne
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
(library (semtok)
  (export
   semtok:classify-full
   semtok:classify-no-modifiers
   semtok:encode
   semtok:list->modifiers
   semtok:modifiers
   semtok:modifiers->flags
   semtok:modifiers-list
   semtok:type->index
   semtok:types-list
   )
  (import
   (chezscheme)
   (config-params)
   (indent)
   (read)
   (swish imports)
   )
  (define-enumeration token-type-element
    (
     comment
     function
     keyword
     macro
     number
     regexp
     string
     type
     variable
     )
    semtok:types)

  (define-enumeration token-modifier-element
    (
     internal
     optimize2
     optimize3
     side-effect
     )
    semtok:modifiers)

  (define semtok:type->index (enum-set-indexer (semtok:types)))

  (define (semtok:modifiers->flags mods)
    (if (not mods)
        0
        (#%$enum-set-members mods)))

  (define (semtok:types-list)
    (enum-set->list (enum-set-universe (semtok:types))))

  (define (semtok:modifiers-list)
    (enum-set->list (enum-set-universe (semtok:modifiers))))

  (define (semtok:list->modifiers ls)
    ((enum-set-constructor (semtok:modifiers)) ls))

  (define defn-regexp
    (let ([local (make-process-parameter #f)])
      (lambda ()
        (or (local)
            (let ([v (pregexp
                      (format "^(?:trace-)?(?:~a)(?:-[\\S]+)?$"
                        (join (cons "define" (map pregexp-quote (config:definition-keywords))) #\|)))])
              (local v)
              v)))))

  (define (last-char s)
    (string-ref s (- (string-length s) 1)))

  (define (type? t)
    (let ([raw (token-raw t)])
      (and (> (string-length raw) 1)
           (eq? (string-ref raw 0) #\<)
           (eq? (last-char raw) #\>))))

  (define-syntax (define-table x)
    (syntax-case x (=>)
      [(_ name ([key ...] => category) ...)
       (andmap identifier? #'(key ... ... category ...))
       (with-syntax ([ref (compound-id #'name #'name "-ref")])
         #'(module (name ref)
             (define name
               (let ([ht (make-eq-hashtable)])
                 (let ([cat 'category])
                   (#3%eq-hashtable-set! ht 'key cat)
                   ...)
                 ...
                 ;; make the table immutable
                 (hashtable-copy ht)))
             (define (ref x)
               (#3%eq-hashtable-ref name x #f))))]))

  (define-table keywords
    ([let trace-let trace-lambda] => let)
    ([define-syntax trace-define-syntax] => define-syntax)
    ([case-lambda
      lambda
      let* letrec letrec* let-values let*-values
      syntax syntax-case syntax-rules let-syntax letrec-syntax
      fluid-let fluid-let-syntax
      library import export only except prefix rename
      include meta-cond
      ] => unnamed)
    ;; HACK These categories are hacks to approximate what might
    ;; follow alias and meta. Not sure what this should really be.
    ([alias] => define+)
    ([meta] => lparen)
    )

  (define (semtok:classify-full t state)
    ;; state: #f | lparen | regexp | define | define+ | let | define-syntax | define-syntax+
    ;;
    ;; patterns:
    ;; (re <string> | (pregexp* <string> => regexp
    ;; (define (<symbol> => function
    ;; (define <symbol> => variable
    ;; (define-syntax (<symbol> => macro
    ;; (define-syntax <symbol> => macro
    ;; (let <symbol> => function
    (define type (token-type t))
    (cond
     [(memq type '(ws eol)) (values #f #f state)]
     [(has-prop? t 'comment) (values 'comment #f state)]
     [(has-prop? t 'string) (values (if (eq? state 'regexp) 'regexp 'string) #f #f)]
     [(has-prop? t 'char) (values 'string #f #f)]
     [(has-prop? t 'number) (values 'number #f #f)]
     [(type? t) (values 'type #f #f)]
     [(and (eq? type 'atomic)
           (eq? state 'lparen))
      (let ([value (token-value t)]
            [raw (token-raw t)])
        (cond
         [(keywords-ref value) =>
          (lambda (category)
            (values 'keyword #f (if (eq? category 'unnamed) #f category)))]
         [(pregexp-match (defn-regexp) raw)
          (values 'keyword #f 'define)]
         [(or (eq? value 're) (starts-with? raw "pregexp"))
          (values #f #f 'regexp)]
         [else
          (let* ([mods (semtok:modifiers)]
                 [mods
                  (cond
                   [(pregexp-match (re "^#([23])?%") raw) =>
                    (lambda (ls)
                      (enum-set-union mods
                        (match ls
                          [(,_ #f) (semtok:modifiers internal)]
                          [(,_ "2") (semtok:modifiers optimize2)]
                          [(,_ "3") (semtok:modifiers optimize3)])))]
                   [else mods])]
                 [mods
                  (if (char=? (last-char raw) #\!)
                      (enum-set-union mods (semtok:modifiers side-effect))
                      mods)])
            (if (enum-set=? mods (semtok:modifiers))
                (values #f #f #f)
                (values 'function mods #f)))]))]
     [(eq? type 'atomic)
      (values
       (match state
         [define 'variable]
         [define+ 'function]
         [define-syntax 'macro]
         [define-syntax+ 'macro]
         [let 'function]
         [,_ #f])
       #f #f)]
     [(eq? type 'lparen)
      (match state
        [define (values #f #f 'define+)]
        [define-syntax (values #f #f 'define-syntax+)]
        [,_ (values #f #f 'lparen)])]
     [else
      (values #f #f #f)]))

  (define (semtok:classify-no-modifiers t state)
    (let-values ([(class modifiers state) (semtok:classify-full t state)])
      (cond
       [modifiers
        (values #f #f #f)]
       [else
        (values class modifiers state)])))

  (define-tuple <semtok> line char length type modifiers)

  (define (cons*token t rest)
    (match-define `(<semtok> ,line ,char ,length ,type ,modifiers) t)
    (cons* line char length type modifiers rest))

  (define (encode-tokens ls)
    (let lp ([ls ls] [prior #f])
      (match ls
        [() '()]
        [(,t . ,rest)
         (match prior
           [#f (cons*token t (lp rest t))]
           [`(<semtok> ,line ,char)
            (let* ([delta-line (- (<semtok> line t) line)]
                   [delta-start (if (zero? delta-line)
                                    (- (<semtok> char t) char)
                                    (<semtok> char t))]
                   [new (<semtok> copy t
                          [line delta-line]
                          [char delta-start])])
              (cons*token new (lp rest t)))])])))

  (define (text->semtoks text start-line end-line semtok-mode)
    (define classify
      (match semtok-mode
        [no-modifiers semtok:classify-no-modifiers]
        [full semtok:classify-full]))
    (let ([tokens (tokenize text start-line end-line)]
          [table (make-code-lookup-table text)])
      (let outer ([tokens tokens] [state #f] [acc '()])
        (match tokens
          [() (reverse acc)]
          [(,t . ,rest)
           (let-values ([(class modifiers state) (classify t state)])
             (cond
              [class
               (let ()
                 (match-define `(token ,raw ,bfp ,efp) t)
                 (let-values ([(line1 char) (fp->line/char table bfp)]
                              [(line2 _char) (fp->line/char table efp)])
                   (let ([len (token-length t)]
                         [type (semtok:type->index class)]
                         [modifiers (semtok:modifiers->flags modifiers)])
                     (cond
                      [(= line1 line2)
                       (outer rest state
                         (cons (<semtok> make
                                 [line (- line1 1)] ; LSP is 0-based
                                 [char (- char 1)]  ; LSP is 0-based
                                 [length len]
                                 [type type]
                                 [modifiers modifiers])
                           acc))]
                      [else
                       ;; VSCode does not like multi-line tokens.
                       (let inner ([start 0]
                                   [line (- line1 1)] ; LSP is 0-based
                                   [char (- char 1)]  ; LSP is 0-based
                                   [acc acc])
                         (match (pregexp-match-positions (re "[\\n]") raw start)
                           [#f
                            (outer rest state
                              (cons (<semtok> make
                                      [line line]
                                      [char char]
                                      [length (- len start)]
                                      [type type]
                                      [modifiers modifiers])
                                acc))]
                           [((,s . ,e))
                            (inner e (+ line 1) 0
                              (cons (<semtok> make
                                      [line line]
                                      [char char]
                                      [length (- s start)]
                                      [type type]
                                      [modifiers modifiers])
                                acc))]))]))))]
              [else (outer rest state acc)]))]))))

  (define (semtok:encode text start-line end-line semtok-mode)
    (encode-tokens
     (text->semtoks text start-line end-line semtok-mode)))
  )
