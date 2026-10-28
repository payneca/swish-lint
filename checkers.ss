;;; Copyright 2020 Beckman Coulter, Inc.
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

(library (checkers)
  (export
   check-import/export
   check-line-whitespace
   make-external-checker
   make-regexp-checker
   run-optional-checkers
   try-checker
   )
  (import
   (chezscheme)
   (config-params)
   (os)
   (read)
   (swish imports)
   (tower-client)
   (trace))

  ;; A checker that fails is reported as a diagnostic, so the other
  ;; checkers still run and their diagnostics are still published.
  (define (try-checker report thunk)
    (match (try (thunk))
      [`(catch ,reason)
       (report 1 'error "checker failed: ~a"
         ;; The reason may hold a large datum.
         (parameterize ([print-level 4] [print-length 8])
           (exit-reason->english reason)))]
      [,_ (void)]))

  (define (run-optional-checkers uri skip-delay? annotated-code code report)
    (for-each
     (lambda (check)
       (try-checker report
         (lambda ()
           (check uri skip-delay? annotated-code code report))))
     (optional-checkers)))

  (define (check-import/export x report)
    (let lp ([x x])
      (match x
        [`(annotation
           [stripped (library ,_ (export . ,_exports) . ,_rest)]
           [expression (,_library ,_
                        `(annotation
                          [expression (,export.anno . ,exports.anno)])
                        . ,rest)])
         (let* ([exports.anno
                 (filter
                  (lambda (x)
                    (match x
                      [`(annotation [stripped (rename . ,_)]) #f]
                      [,_ #t]))
                  exports.anno)]
                [exports (map annotation-stripped exports.anno)]
                [exports.orig (map (lambda (x) (format "~s" x)) exports)]
                [exports.sorted (sort string<? exports.orig)])
           (unless (equal? exports.orig exports.sorted)
             (report export.anno 'warning "should sort exports")
             (for-each
              (lambda (anno l r)
                (unless (equal? l r)
                  (report anno 'info "incorrectly sorted: ~a" l)))
              exports.anno exports.orig exports.sorted)))
         (lp rest)]
        [`(annotation
           [stripped (import . ,_imports)]
           [expression (,import.anno . ,imports.anno)])
         (let* ([imports.anno
                 (filter
                  (lambda (x)
                    (match x
                      [`(annotation [stripped (,first . ,_)])
                       (guard (memq first '(only except prefix rename)))
                       #f]
                      [,_ #t]))
                  imports.anno)]
                [imports (map annotation-stripped imports.anno)]
                [imports.orig (map (lambda (x) (format "~s" x)) imports)]
                [imports.sorted (sort string<? imports.orig)])
           (unless (equal? imports.orig imports.sorted)
             (report import.anno 'warning "should sort imports")
             (for-each
              (lambda (ann l r)
                (unless (equal? l r)
                  (report ann 'info "incorrectly sorted: ~a" l)))
              imports.anno imports.orig imports.sorted)))]
        [(,first . ,rest)
         (lp first)
         (lp rest)]
        [,_ (void)])))

  (define (check-line-whitespace text collapse? report)
    (define (yucky rlines text)
      (let ([lines (reverse rlines)])
        (cond
         [collapse?
          (match lines
            [() (void)]
            [(,line . ,_)
             (report line 'error "~a~@[ (~a times)~]" text
               (let ([len (length lines)])
                 (and (> len 1) len)))])]
         [else
          (for-each
           (lambda (line)
             (report line 'error "~a" text))
           lines)])))
    (define tabs '())
    (define dos '())
    (define ws '())
    (do ([ln 1 (+ ln 1)]
         [lines (split text #\newline) (cdr lines)])
        ((null? lines))
      (let* ([line (car lines)]
             [len (string-length line)])
        (when (> len 0)
          (let lp ([i 0])
            (when (< i len)
              (if (char=? (string-ref line i) #\tab)
                  (set! tabs (cons ln tabs))
                  (lp (+ i 1)))))
          (let ([last-char (string-ref line (- len 1))])
            (cond
             [(char=? last-char #\return)
              (set! dos (cons ln dos))]
             [(char-whitespace? last-char)
              (set! ws (cons ln ws))])))))
    (yucky tabs "undesirable tab character")
    (yucky dos "undesirable DOS line ending")
    (yucky ws "undesirable trailing whitespace"))

  (define (make-regexp-checker type regexp)
    (define compiled-regexp (pregexp regexp))
    (lambda (uri skip-delay? annotated-code code report)
      (do ([ln 1 (+ ln 1)]
           [lines (split code #\newline) (cdr lines)])
          ((null? lines))
        (let ([line (car lines)])
          ;; Report every match on the line. The first element of a
          ;; result is the whole match; the rest are its groups.
          (let lp ([i 0])
            (match (pregexp-match-positions compiled-regexp line i)
              [#f (void)]
              [((,start . ,end) . ,_)
               (cond
                [(= start end)
                 ;; Skip an empty match so the search moves on.
                 (lp (+ end 1))]
                [else
                 ;; start and end are 0-based, but #(range ...) is 1-based
                 (report `#(range ,ln ,(+ start 1) ,ln ,(+ end 1))
                   type
                   "~a"
                   (substring line start end))
                 (lp end)])]))))))

  (define (make-external-checker ls)
    (lambda (uri skip-delay? annotated-code code report)
      ;; Returns what the program reported, in order: a JSON object for
      ;; each diagnostic, and a string for each other nonblank line.
      (define (read-output ip)
        (let lp ([acc '()])
          (let ([c (peek-char ip)])
            (cond
             [(eof-object? c) (reverse acc)]
             [(char-whitespace? c)
              (get-char ip)
              (lp acc)]
             [(char=? c #\{)
              (lp (cons (json:read ip) acc))]
             [else
              (let ([inp (trim-whitespace (get-line ip))])
                (lp (if (string=? inp "") acc (cons inp acc))))]))))
      (define (trace-stderr ip)
        (let lp ()
          (let ([line (get-line ip)])
            (unless (eof-object? line)
              (display line (trace-output-port))
              (newline (trace-output-port))
              (lp)))))
      ;; Reports what read-output returned, and returns how many
      ;; diagnostics that was.
      (define (report-all items)
        (let lp ([items items] [lookup-table #f] [reported 0])
          (match items
            [() reported]
            [(,str . ,rest)
             (guard (string? str))
             (let ([lookup-table (or lookup-table (make-code-lookup-table code))])
               (let-values ([(line msg) (reason->line/msg str lookup-table)])
                 (report line 'error "~a" msg)
                 (lp rest lookup-table (+ reported 1))))]
            [(,obj . ,rest)
             (let* ([msg (json:ref obj 'message #f)]
                    [type (match (json:ref obj 'type #f)
                            ["info" 'info]
                            ["hint" 'hint]
                            ["warning" 'warning]
                            [,_ 'error])]
                    [line (json:ref obj 'line #f)]
                    [line (and line (fixnum? line) (fxpositive? line) line)]
                    [col (json:ref obj 'column #f)]
                    [col (and col (fixnum? col) (fxpositive? col) col)])
               (cond
                [msg
                 (report `#(near ,code ,line ,col) type "~a" msg)
                 (lp rest lookup-table (+ reported 1))]
                [else
                 (lp rest lookup-table reported)]))])))
      (define (run-command cmd filename)
        ;; A failed run gets one diagnostic for the user, and the reason
        ;; goes to the log. The checker's own stderr goes to the trace.
        (define (failed why)
          (fprintf (console-error-port)
            "swish-lint: external checker ~a failed on ~a: ~a\n"
            (car cmd) filename why)
          (flush-output-port (console-error-port))
          (report 1 'warning "external checker ~a failed; see the log"
            (path-last (car cmd))))
        (trace-time `(run-external-checker ,@cmd)
          (match
           (try
            (os:run (car cmd) (cdr cmd)
              (os:run-options
               [stdout read-output]
               [stderr trace-stderr]
               [idle-timeout 5000])))
           [`(catch #(os-process-start-failed ,_ ,reason))
            (failed (format "couldn't start: ~a" (exit-reason->english reason)))]
           [`(catch #(os-process-timeout idle-timeout ,_ ,_))
            (failed "produced no output for 5 s")]
           [`(catch ,reason)
            ;; Its output couldn't be read, e.g. bad JSON.
            (failed (exit-reason->english reason))]
           [`(<os:result> ,stdout ,exit-status ,term-signal)
            (let ([reported (report-all stdout)])
              (cond
               [(not (eqv? term-signal 0))
                (failed (format "was stopped by signal ~a" term-signal))]
               [(and (not (eqv? exit-status 0)) (= reported 0))
                ;; Programs like this exit nonzero when they report
                ;; problems, so that alone isn't a failure.
                (failed (format "exited with status ~a" exit-status))]))])))
      (when skip-delay?                 ; recently saved
        (let* ([filename
                (if (starts-with? uri "file:")
                    (uri->abs-path uri)
                    uri)]
               [cmd (fold-right
                     (lambda (x acc)
                       (and acc
                            (match x
                              [filename (cons filename acc)]
                              [(filename ,re)
                               (if (pregexp-match re (path-last filename))
                                   (cons filename acc)
                                   #f)]
                              [,_ (cons x acc)])))
                     '()
                     ls)])
          (when cmd       ; skip the file if the filename regexp fails
            (run-command cmd filename))))))
  )
