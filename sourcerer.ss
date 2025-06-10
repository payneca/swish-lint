#!chezscheme
(library (sourcerer)
  (export
   sourcerer:file-saved
   sourcerer:import
   sourcerer:root-dir
   sourcerer:start&link
   sourcerer:walk-refs
   )
  (import
   (chezscheme)
   (os-process)
   (swish imports)
   (trace)
   )
  (include "hack-record-types.ss")

  (define-tuple <sm>
    st-dump
    prim*
    node*
    rubbish
    )

  (define-syntax foreach
    (syntax-rules ()
      [(_ ([var collection*] ...) e0 e1 ...)
       (let ([f (lambda (var ...) e0 e1 ...)]
             [var collection*] ...)
         (cond
          [(and (vector? var) ...) (vector-for-each f var ...)]
          [else (for-each f var ...)]))]))

  (define (find/run ht key proc)
    (let ([v (hashtable-ref ht key #!bwp)])
      (cond
       [(not (eq? v #!bwp)) v]
       [else
        (let ([v (proc)])
          (hashtable-set! ht key v)
          v)])))

  (define (load-source-map filename)
    (and (file-exists? filename)
         (let ([ip (open-binary-file-to-read filename)])
           (on-exit (close-port ip)
             (fasl-read ip)))))

  (define (sourcerer:import filename base-dir)
    (define ->uid (make-eq-hashtable))
    (define next-int 0)
    (define (get-next-int)
      (set! next-int (+ next-int 1))
      next-int)
    (define (get-uid key name)
      (find/run ->uid key
        (lambda () (get-next-int))))
    (define visited-sfd (make-hashtable string-hash string=?))
    (define visited-srcs (make-hashtable string-hash string=?))
    (define (sfd-key fn cs) (format "~a:~a" fn cs))
    (define (src-key fn cs bfp efp) (format "~a:~a:~a:~a" fn cs bfp efp))
    (define (add-ref name uid ref-type type src)
      (when src
        (let* ([sfd (source-object-sfd src)]
               [fn (source-file-descriptor-path sfd)]
               [fn (if (path-absolute? fn)
                       fn
                       (path-combine base-dir fn))]
               [fn (coerce fn)]
               [cs (coerce (source-file-descriptor-checksum sfd))]
               [bfp (coerce (source-object-bfp src))]
               [efp (coerce (source-object-efp src))])
          (find/run visited-sfd (sfd-key fn cs)
            (lambda ()
              (db:log 'log-db "INSERT OR IGNORE INTO sfds (filename, checksum) VALUES (?, ?)" fn cs)
              #t))
          (find/run visited-srcs (src-key fn cs bfp efp)
            (lambda ()
              (db:log 'log-db
                (ct:join #\space
                  "INSERT OR IGNORE INTO sources (sfd_fk, bfp, efp)"
                  "VALUES ("
                  "(SELECT sfd_pk FROM sfds WHERE filename = ?1 AND checksum = ?2),"
                  "?3, ?4"
                  ")")
                fn cs bfp efp)
              #t))
          (db:log 'log-db
            (ct:join #\space
              "INSERT INTO ref_src (source_fk, name, uid, ref_type, type)"
              "VALUES ("
              "  ("
              "    SELECT source_pk FROM sources"
              "    WHERE sfd_fk = (SELECT sfd_pk FROM sfds WHERE filename = ?1 AND checksum = ?2)"
              "      AND bfp = ?3 AND efp = ?4"
              "  ),"
              "  ?5,"
              "  ?6,"
              "  ?7,"
              "  ?8"
              ")")
            fn cs bfp efp (coerce name) (coerce uid) (coerce ref-type) (coerce type)))))
    (match (load-source-map filename)
      [#f #f]
      [#!eof #f]
      [`(<sm> ,st-dump ,prim* ,node* ,rubbish)
       (foreach ([node node*])
         (match node
           [`(identifier-info ,name ,kind ,def ,set* ,ref*)
            (define uid (get-uid node name))
            (add-ref name uid kind 'bind def)
            (foreach ([src set*]) (add-ref name uid kind 'set src))
            (foreach ([src ref*]) (add-ref name uid kind 'ref src))]
           [,_ (void)]))
       (foreach ([prim prim*])
         (match prim
           [(,name [safe ,safe-src*] [unsafe ,unsafe-src*])
            (define uid (get-uid prim name))
            (foreach ([src safe-src*]) (add-ref name uid 'safe-prim 'ref src))
            (foreach ([src unsafe-src*]) (add-ref name uid 'unsafe-prim 'ref src))]))
       #t]))

  (define (sourcerer:walk-refs filename table proc)
    #f)

  (define (sourcerer:start&link)
    (define-state-tuple <sourcerer> root-dir os-pid os-monitor)
    (define (process-input ip pid)  ; TODO what should this really do?
      (let ([line (get-line ip)])
        (unless (eof-object? line)
          (display line (trace-output-port))
          (newline (trace-output-port))
          (process-input ip pid))))
    (define (process-stderr ip pid)
      (let ([line (get-line ip)])
        (unless (eof-object? line)
          (display line (trace-output-port))
          (newline (trace-output-port))
          (process-stderr ip pid))))
    (define (init)
      `#(ok
         ,(<sourcerer> make
            [root-dir #f]
            [os-pid #f]
            [os-monitor #f]
            )))
    (define (terminate reason state)
      (cond
       [($state os-pid) =>
        (lambda (os-pid) (os-process:stop os-pid 0))]))
    (define (handle-call msg from state) (match msg))
    (define (handle-cast msg state)
      (match msg
        [#(file-saved ,fn)
         (trace-expr
          `((file-saved ,fn)
            (root-dir ,($state root-dir))))
         `#(no-reply ,state 1000)]
        [#(root-dir ,dir)
         `#(no-reply ,($state copy [root-dir dir]) 1000)]))
    (define (handle-info msg state)
      (match msg
        [timeout
         (trace-expr 'sourcerer:timeout)
         (assert ($state root-dir))     ; TODO remove
         (assert (not ($state os-pid))) ; TODO remove
         (let ([prep.ss (path-combine ($state root-dir) ".swish" "prep.ss")])
           (cond
            [(file-exists? prep.ss)
             (trace-expr `(sourcerer:found ,prep.ss))
             (match (os-process:start&link "prep"
                      ;; TODO don't trust that prep runs from the
                      ;; right directory. Should update it to take an
                      ;; optional root directory to cd to before
                      ;; evaling.
                      (list (format "(load \"~a\")" prep.ss))
                      'utf8
                      process-input
                      #f
                      process-stderr)
               [#(error ,reason)
                (trace-expr `(sourcerer ,(exit-reason->english reason)))
                `#(no-reply ,state)]
               [#(ok ,pid)
                `#(no-reply ,($state copy [os-pid pid] [os-monitor (monitor pid)]))])]
            [else
             `#(no-reply ,state)]))]
        [`(DOWN ,m ,_ ,reason)
         (cond
          [(eq? m ($state os-monitor))
           (trace-expr `(sourcerer:exited ,(exit-reason->english reason)))
           `#(no-reply ,($state copy [os-pid #f] [os-monitor #f]))]
          [else
           `#(no-reply ,state)])]))
    (gen-server:start&link 'sourcerer))

  (define (sourcerer:root-dir dir)
    (gen-server:cast 'sourcerer `#(root-dir ,dir)))

  (define (sourcerer:file-saved fn)
    (gen-server:cast 'sourcerer `#(file-saved ,fn)))
  )
