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
 (tower-client))

(isolate-mat uri->abs-path ()
  (match-let*
   (["/tmp/a.ss" (uri->abs-path "file:///tmp/a.ss")]
    ["/home/u/my proj/a.ss" (uri->abs-path "file:///home/u/my%20proj/a.ss")]
    ["/tmp/a#b%c.ss" (uri->abs-path "file:///tmp/a%23b%25c.ss")]
    ["/tmp/café.ss" (uri->abs-path "file:///tmp/caf%C3%A9.ss")]
    ["/tmp/café.ss" (uri->abs-path "file:///tmp/caf%c3%a9.ss")]
    ;; Characters that should have been encoded are accepted.
    ["/tmp/a b.ss" (uri->abs-path "file:///tmp/a b.ss")]
    ;; A % that doesn't start a valid sequence is kept.
    ["/tmp/100%.ss" (uri->abs-path "file:///tmp/100%.ss")]
    ["/tmp/%zz%4.ss" (uri->abs-path "file:///tmp/%zz%4.ss")]
    [`(catch #(unhandled-uri "untitled:Untitled-1"))
     (try (uri->abs-path "untitled:Untitled-1"))])
   'ok))

(isolate-mat abs-path->uri ()
  (match-let*
   (["file:///tmp/a.ss" (abs-path->uri "/tmp/a.ss")]
    ["file:///tmp/A-z_0.9~.ss" (abs-path->uri "/tmp/A-z_0.9~.ss")]
    ["file:///home/u/my%20proj/a.ss" (abs-path->uri "/home/u/my proj/a.ss")]
    ["file:///tmp/a%23b%25c%3Fd.ss" (abs-path->uri "/tmp/a#b%c?d.ss")]
    ["file:///tmp/caf%C3%A9.ss" (abs-path->uri "/tmp/café.ss")]
    ["file:///tmp/%F0%9F%98%80.ss" (abs-path->uri "/tmp/\x1F600;.ss")])
   'ok))

(isolate-mat uri-round-trip ()
  (for-each
   (lambda (path)
     (assert (equal? path (uri->abs-path (abs-path->uri path)))))
   '("/tmp/a.ss"
     "/home/u/my proj/a b.ss"
     "/tmp/a#b%c?d[e]f@g:h!i$j&k'l(m)n*o+p,q;r=s.ss"
     "/tmp/café/\x1F600;.ss")))

(isolate-mat windows-drives ()
  (meta-cond
   [windows?
    (match-let*
     (["C:/Users/a b/x.ss" (uri->abs-path "file:///c%3A/Users/a%20b/x.ss")]
      ["C:/Users/x.ss" (uri->abs-path "file:///C:/Users/x.ss")]
      ["file:///c%3A/Users/a%20b/x.ss" (abs-path->uri "C:\\Users\\a b\\x.ss")]
      ["file:///c%3A/Users/a%20b/x.ss" (abs-path->uri "C:/Users/a b/x.ss")])
     'ok)]
   [else (void)]))
