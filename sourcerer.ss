#!chezscheme
(library (sourcerer)
  (export
   sourcerer:walk-refs
   )
  (import
   (chezscheme)
   (swish imports)
   )
  ;; TODO decide out how to provide access to compatible record types for client's use
  (define-record-type lexical-info
    (nongenerative #{lexical-info ble5klpzns025alnatm0ydav9-0})
    (fields
     (immutable name)
     (immutable bind-src)
     (mutable ref-src*)
     (mutable set-src*)))

  ;; TODO do we care about meta-level for globals?
  (define-record-type global-info
    (nongenerative #{global-info ble5klpzns025alnatm0ydav9-1})
    (fields
     (immutable name)
     (mutable ref-src*)
     (mutable set-src*)))

  ;; TODO better names? don't want to confuse with make-priminfo elsewhere
  (define-record-type prim-info
    (nongenerative #{prim-info a9h3n8t2pis427wy51x6e77bg-0})
    (fields
     (immutable name)
     (mutable ref2-src*)
     (mutable ref3-src*)))

  (define-record-type syntax-info
    (nongenerative #{syntax-info ble5klpzns025alnatm0ydav9-3})
    (fields
     (immutable name)
     (immutable bind-src)
     (immutable meta-level)
     (mutable ref-src*)))

  (define-record-type contour
    (nongenerative #{contour ble5klpzns025alnatm0ydav9-4})
    (fields
     (immutable src)
     (immutable type)
     (immutable meta-level)
     (immutable bound*)))

  (define-record-type realm
    (nongenerative #{realm ble5klpzns025alnatm0ydav9-5})
    (fields
     (immutable src)
     (immutable name)
     (immutable path)
     (immutable version)
     (immutable meta-level)
     (immutable export*)
     (immutable import*)))

  (define (sourcerer:walk-refs filename table proc)
    (define ->uid (make-eq-hashtable))
    (define next-int 0)
    (define (get-next-int)
      (set! next-int (+ next-int 1))
      next-int)
    (define (get-uid info name)
      (cond
       [(eq-hashtable-ref ->uid info #f)]
       [else
        (let ([uid (format "~a~a" name (get-next-int))])
          (eq-hashtable-set! ->uid info uid)
          uid)]))
    (define (guarded type name uid src)
      (when src
        (let* ([sfd (source-object-sfd src)]
               [path (and sfd (source-file-descriptor-path sfd))])
          (when (and path
                     (string=? (path-last path) (path-last filename))) ; HACK
            (proc table name uid type src)))))

    (when (file-exists? "/tmp/source-map.fasl")
      (let ([ip (open-binary-file-to-read "/tmp/source-map.fasl")])
        (on-exit (close-port ip)
          (let lp ()
            (let* ([cat (fasl-read ip)]
                   [data (fasl-read ip)])
              (match cat
                [#!eof (void)]
                [lexical
                 (vector-for-each
                  (lambda (info)
                    (match info
                      [`(lexical-info ,name ,bind-src ,ref-src* ,set-src*)
                       (define uid (get-uid info name))
                       (define (ref! src) (guarded 'lexical name uid src))
                       (ref! bind-src)
                       (for-each ref! ref-src*)
                       (for-each ref! set-src*)]))
                  data)
                 (lp)]
                [global
                 (vector-for-each
                  (lambda (info)
                    (match info
                      [`(global-info ,name ,ref-src* ,set-src*)
                       ;; global-info's name is a gensym. We can use
                       ;; that for our unique id, but need to get a
                       ;; pretty name for the rest of the system.
                       (define uid (format "~s" name))
                       (let ([name (parameterize ([print-gensym #f]) (format "~s" name))])
                         (define (ref! src) (guarded 'global name uid src))
                         (for-each ref! ref-src*)
                         (for-each ref! set-src*))]))
                  data)
                 (lp)]
                [prim
                 (vector-for-each
                  (lambda (info)
                    (match info
                      [`(prim-info ,name ,ref2-src* ,ref3-src*)
                       (define uid (get-uid info name))
                       (define (ref! src) (guarded 'prim name uid src))
                       (for-each ref! ref2-src*)
                       (for-each ref! ref3-src*)]))
                  data)
                 (lp)]
                [syntax
                 (vector-for-each
                  (lambda (info)
                    (match info
                      [`(syntax-info ,name ,bind-src ,ref-src*)
                       (define uid (get-uid info name))
                       (define (ref! src)
                         (guarded 'syntax name uid
                           (cond
                            ;; Built in syntax are marked. For now,
                            ;; pretend like we just don't have source.
                            [(eq? src 'built-in) #f]
                            [else src])))
                       (ref! bind-src)
                       (for-each ref! ref-src*)]))
                  data)
                 (lp)]
                [,_ (lp)])))))))
  )
