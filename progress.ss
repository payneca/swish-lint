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

(library (progress)
  (export
   progress:created
   progress:inc-done
   progress:inc-total
   progress:not-created
   progress:start
   )
  (import
   (chezscheme)
   (swish imports)
   )
  ;; Unless created? is false, the client already knows the progress
  ;; token. Otherwise messages wait until progress:created, or are
  ;; dropped after progress:not-created.
  (define progress:start
    (case-lambda
     [(title render-msg send-msg trace-expr)
      (progress:start title render-msg send-msg trace-expr #t)]
     [(title render-msg send-msg trace-expr created?)
      (start title render-msg send-msg trace-expr created?)]))

  (define (start title render-msg send-msg trace-expr created?)
    (define-state-tuple <progress>
      start-time
      wake-time
      max-reported
      total
      done
      pending                  ; created | dropped | queue of messages
      )
    (define (no-reply state)
      `#(no-reply ,state ,(or ($state wake-time) 'infinity)))
    (define (wake-at old)
      (or old (+ (erlang:now) 100)))
    ;; Sends msg, or keeps it until the token is created. Returns the
    ;; new state.
    (define (emit msg state)
      (match ($state pending)
        [created (send-msg msg) state]
        [dropped state]
        [,waiting ($state copy [pending (queue:add msg waiting)])]))
    (define (started state)
      (if ($state start-time)
          state
          (let ([state (emit
                        (json:make-object
                         [kind "begin"]
                         [title title]
                         [percentage 0])
                        state)])
            ($state copy [start-time (erlang:now)]))))

    (define (init)
      `#(ok
         ,(<progress> make
            [start-time #f]
            [wake-time #f]
            [max-reported 0]
            [total 0]
            [done 0]
            [pending (if created? 'created queue:empty)])))
    (define (terminate reason state)
      ($state open [start-time])
      (when start-time
        (emit (json:make-object [kind "end"]) state)
        (trace-expr `(time ,title ,(- (erlang:now) start-time) ms))))
    (define (handle-call msg from state) (match msg))
    (define (handle-cast msg state)
      (match msg
        [inc-total
         (let ([state (started state)])
           (no-reply
            ($state copy*
              [wake-time (wake-at wake-time)]
              [total (fx+ total 1)])))]
        [inc-done
         (let ([state (started state)])
           (no-reply
            ($state copy*
              [wake-time (wake-at wake-time)]
              [done (fx+ done 1)])))]
        [created
         (match ($state pending)
           [created (no-reply state)]
           [dropped (no-reply state)]
           [,waiting
            (let lp ([q waiting])
              (unless (queue:empty? q)
                (send-msg (queue:get q))
                (lp (queue:drop q))))
            (no-reply ($state copy [pending 'created]))])]
        [not-created
         (no-reply ($state copy [pending 'dropped]))]))
    (define (handle-info msg state)
      (match msg
        [timeout
         (<progress> open state [max-reported total done])
         (let ([msg (render-msg done total)]
               [percent
                (max max-reported
                     (min 100
                          (if (zero? total)
                              0
                              (floor (* (/ done total) 100)))))])
           (let* ([state (emit
                          (json:make-object
                           [kind "report"]
                           [message msg]
                           [percentage percent])
                          state)]
                  [state ($state copy [max-reported percent] [wake-time #f])])
             (if (< done total)
                 (no-reply state)
                 `#(stop normal ,state))))]))
    (gen-server:start #f))

  (define (progress:inc-total who)
    (gen-server:cast who 'inc-total))

  (define (progress:inc-done who)
    (gen-server:cast who 'inc-done))

  (define (progress:created who)
    (gen-server:cast who 'created))

  (define (progress:not-created who)
    (gen-server:cast who 'not-created))
  )
