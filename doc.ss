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
   doc:get-text
   doc:get-value-near
   doc:start
   doc:start&link
   doc:updated
   )
  (import
   (chezscheme)
   (cursor)
   (read)
   (swish imports)
   (trace)
   )

  (define-state-tuple <document> cursor worker-pid on-changed)

  (define (init on-changed)
    `#(ok ,(<document> make
             [cursor #f]
             [worker-pid #f]
             [on-changed on-changed])))

  (define (terminate reason state) 'ok)

  (define (handle-call msg from state)
    (match msg
      [get-text
       `#(reply ,(cursor->string ($state cursor)) ,state)]
      [#(get-value-near ,line1 ,char1)
       (trace-time `(get-value-near ,line1 ,char1)
         (let* ([cursor (cursor:goto-line! ($state cursor) (fx- line1 1))]
                [text (line-str (cursor-line cursor))])
           (define (->string value bfp efp)
             (let ([offset (match value
                             [,_ (guard (symbol? value)) 0]
                             [($primitive ,value) 2]
                             [($primitive ,_ ,value) 3]
                             [,_ #f])])
               (and offset
                    (substring text (+ bfp offset) efp))))
           (match (try
                   (let-values ([(type value bfp efp)
                                 (read-token-near/col text char1)])
                     (and (eq? type 'atomic)
                          (->string value bfp efp))))
             [`(catch ,reason)
              (trace-expr
               `(get-value-near ,line1 ,char1 => ,(exit-reason->english reason)))
              `#(reply #f ,state)]
             ["" `#(reply #f ,state)]
             [,result
              `#(reply ,result ,state)])))]))

  (define (handle-cast msg state)
    (match msg
      [#(updated ,change ,skip-delay?)
       (let ([pid ($state worker-pid)])
         (when pid (kill pid 'cancelled)))
       (let ([cursor
              (cond
               [(not change) (string->cursor "")]
               [(string? change) (string->cursor change)]
               [else (lsp:change-content ($state cursor) change)])])
         (cond
          [($state on-changed) =>
           (lambda (on-changed)
             (let ([pid (on-changed change (cursor:copy cursor) skip-delay?)])
               (monitor pid)
               `#(no-reply
                  ,($state copy
                     [cursor cursor]
                     [worker-pid pid]))))]
          [else
           `#(no-reply ($state copy [cursor cursor]))]))]))

  (define (handle-info msg state)
    (match msg
      [`(DOWN ,_ ,pid ,reason)
       (cond
        [(eq? pid ($state worker-pid))
         (unless (eq? reason 'normal)
           (trace-expr `(doc-worker ,(exit-reason->english reason))))
         `#(no-reply ,($state copy [worker-pid #f]))]
        [else
         `#(no-reply ,state)])]))

  (define (doc:start&link on-changed)
    (gen-server:start&link #f on-changed))

  (define (doc:start on-changed)
    (gen-server:start #f on-changed))

  (define (doc:get-text who)
    (gen-server:call who 'get-text))

  (define (doc:get-value-near who line1 char1)
    (gen-server:call who `#(get-value-near ,line1 ,char1)))

  (define (doc:updated who change skip-delay?)
    (gen-server:cast who `#(updated ,change ,skip-delay?)))
  )
