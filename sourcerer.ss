#!chezscheme
(library (sourcerer)
  (export
   sourcerer:import
   sourcerer:walk-refs
   )
  (import
   (chezscheme)
   (swish imports)
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
  )
