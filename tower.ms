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

(import
 (json)
 (tower))

(define (start-tower)
  (log-file ":memory:")
  (match-let*
   ([#(ok ,_)
     (supervisor:start&link 'main-sup 'one-for-all 0 1
       (append
        (make-swish-sup-spec (list swish-event-logger))
        (tower:sup-spec 0)))])
   (http:get-port-number 'http)))

(define (stop-tower)
  (transaction 'log-db 'ok)
  (let ([pid (whereis 'main-sup)])
    (unlink pid)
    (monitor pid)
    (kill pid 'shutdown)
    (receive (after 15000 (throw 'timeout-in-stop))
      [`(DOWN ,_ ,@pid ,_) 'ok])))

(define (update-references id filename refs)
  (json:object->bytevector
   (json:make-object
    [jsonrpc "2.0"]
    [id id]
    [method "update-references"]
    [params
     (json:make-object
      [filename filename]
      [references refs])])))

(define (make-refs prefix n)
  (let lp ([i 0] [acc '()])
    (if (= i n)
        acc
        (lp (+ i 1)
          (cons
           (json:make-object
            [name (format "~a~a" prefix i)]
            [line i]
            [char 0]
            [len 4]
            [meta (json:make-object)])
           acc)))))

(define (file-ref-count filename)
  (transaction 'log-db
    (match (execute "
select count(*) from refs R
inner join files F on F.file_pk=R.file_fk
where F.filename=?" filename)
      [(#(,n)) n])))

(isolate-mat concurrent-update ()
  ;; Two connections replace the same file's references at once. The
  ;; file must end up with one set, not a mix of both.
  (let ([port (start-tower)]
        [filename "/tmp/concurrent.ss"]
        [n 20000])
    (on-exit (stop-tower)
      (let ([ws1 (ws:connect "localhost" port "/tower" self)]
            [ws2 (ws:connect "localhost" port "/tower" self)]
            [msg1 (update-references 1 filename (make-refs "a" n))]
            [msg2 (update-references 2 filename (make-refs "b" n))])
        (ws:send ws1 msg1)
        (ws:send ws2 msg2)
        (let lp ([replies 0])
          (when (< replies 2)
            (receive
             [#(ws:message ,_ ,_) (lp (+ replies 1))]
             [#(ws:init ,_) (lp replies)])))
        (ws:close ws1)
        (ws:close ws2)
        (assert (= n (file-ref-count filename)))))))
