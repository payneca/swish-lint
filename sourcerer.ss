#!chezscheme
(library (sourcerer)
  (export
   sourcerer:import
   sourcerer:walk-refs
   )
  (import
   (chezscheme)
   (read)
   (swish imports)
   (trace)
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
    (nongenerative #{realm dk0h38d9wcwydof3f2dgd7w9h-0})
    (fields
     (immutable src)
     (immutable name)
     (immutable path)
     (immutable version)
     (immutable meta-level)
     (immutable export*)
     (immutable import*)
     (immutable export-id*)))

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
          (when (and path (string=? filename path)) ; HACK still a hack, but less trouble
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
                       (guarded 'bind name uid bind-src)
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
                       (define uid (format "~s" name))
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
                       (define (protect src)
                         (cond
                          ;; Built in syntax are marked. For now,
                          ;; pretend like we just don't have source.
                          [(eq? src 'built-in) #f]
                          [else src]))
                       (define (ref! src)
                         (guarded 'syntax name uid (protect src)))
                       (guarded 'bind name uid (protect bind-src))
                       (for-each ref! ref-src*)]))
                  data)
                 (lp)]
                [realm
                 (for-each
                  (lambda (info)
                    (match info
                      [`(realm ,export-id*)
                       (for-each
                        (lambda (ex)
                          (match ex
                            [(,name . `(annotation ,source ,stripped))
                             (define uid (format "~s" name))
                             (let ([name (parameterize ([print-gensym #f]) (format "~s" name))])
                               (guarded 'global name uid source))]
                            #;[(,global-name . ,lexical-name)
                               (define uid (format "~s" global-name))
                               (guarded 'global lexical-name uid #f)]
                            [,_ (trace-expr `(unhandled-realm-export-id* ,ex))]
                            ))
                        export-id*)]))
                  data)
                 (lp)]
                [contour (lp)]
                [imports-ht (lp)]
                [alias (lp)])))))))

  (define (process-file filename p)
    (when (file-exists? filename)
      (let ([ip (open-binary-file-to-read filename)])
        (on-exit (close-port ip)
          (let lp ()
            (let* ([cat (fasl-read ip)]
                   [data (fasl-read ip)])
              (unless (eof-object? cat)
                (p cat data)
                (lp))))))))

  (define (sourcerer:import filename)
    #|

select R.*, S.*, F.*
from ref_src R, sources S, sfds F
where R.source_fk=S.source_pk
  and S.sfd_fk = F.sfd_pk
  and R.[type]='set'
order by filename, bfp

select distinct *, count(*)
from ref_src
group by source_fk, name, ref_type, type
order by count(*) desc

    |#

    (define src->sfd-pks (make-hashtable string-hash string=?))
    (define src->src-pks (make-hashtable string-hash string=?))

    (define (sfd-key fn cs) (format "~a:~a" fn cs))
    (define (src-key fn bfp efp) (format "~a:~a:~a" fn bfp efp))

    (define (ensure-src src)
      (and src
           (let* ([sfd (source-object-sfd src)]
                  [fn (coerce (source-file-descriptor-path sfd))]
                  [cs (coerce (source-file-descriptor-checksum sfd))]
                  [bfp (coerce (source-object-bfp src))]
                  [efp (coerce (source-object-efp src))]
                  [skey (src-key fn bfp efp)])
             (cond
              [(hashtable-ref src->src-pks skey #f)]
              [else
               (let* ([fkey (sfd-key fn cs)]
                      [sfd-fk
                       (cond
                        [(hashtable-ref src->sfd-pks fkey #f)]
                        [else
                         (execute "insert into sfds(filename,checksum) values(?,?)" fn cs)
                         (match (execute "select sfd_pk from sfds where filename=? and checksum=?" fn cs)
                           [(#(,pk))
                            (hashtable-set! src->sfd-pks fkey pk)
                            pk])])])
                 (execute "insert into sources(sfd_fk,bfp,efp) values(?,?,?)" sfd-fk bfp efp)
                 (match (execute "select source_pk from sources where sfd_fk=? and bfp=? and efp=?" sfd-fk bfp efp)
                   [(#(,pk))
                    (hashtable-set! src->src-pks skey pk)
                    pk]))]))))

    (define (add-ref name ref-type type src)
      (let ([src-fk (ensure-src src)])
        (when src-fk
          (execute "insert into ref_src(name,ref_type,type,source_fk) values(?,?,?,?)"
            (coerce name) (coerce ref-type) (coerce type) src-fk))))

    (transaction 'log-db
      (process-file filename
        (lambda (cat data)
          (match cat
            [lexical
             (vector-for-each
              (lambda (info)
                (match info
                  [`(lexical-info ,name ,bind-src ,ref-src* ,set-src*)
                   (add-ref name 'lexical 'bind bind-src)
                   (for-each (lambda (src) (add-ref name 'lexical 'ref src)) ref-src*)
                   (for-each (lambda (src) (add-ref name 'lexical 'set src)) set-src*)]))
              data)]
            [global
             (vector-for-each
              (lambda (info)
                (match info
                  [`(global-info ,name ,ref-src* ,set-src*)
                   (for-each (lambda (src) (add-ref name 'global 'ref src)) ref-src*)
                   (for-each (lambda (src) (add-ref name 'global 'set src)) set-src*)]))
              data)]
            [prim
             (vector-for-each
              (lambda (info)
                (match info
                  [`(prim-info ,name ,ref2-src* ,ref3-src*)
                   (for-each (lambda (src) (add-ref name 'prim2 'ref src)) ref2-src*)
                   (for-each (lambda (src) (add-ref name 'prim3 'ref src)) ref3-src*)]))
              data)]
            [syntax
             (vector-for-each
              (lambda (info)
                (match info
                  [`(syntax-info ,name ,bind-src ,ref-src*)
                   (define (protect src)
                     (cond
                      ;; Built in syntax are marked. For now,
                      ;; pretend like we just don't have source.
                      [(eq? src 'built-in) #f]
                      [else src]))
                   (add-ref name 'syntax 'bind (protect bind-src))
                   (for-each (lambda (src) (add-ref name 'syntax 'ref (protect src))) ref-src*)]))
              data)]
            [realm 'ok]
            [contour 'ok]
            [imports-ht 'ok]
            [alias 'ok])))

      ;; Need to alo ensure we have the fp -> line/col table
      (execute "delete from line_fps where sfd_fk in (select sfd_pk from sfds)")
      (for-each
       (lambda (row)
         (match row
           [#(,fk ,filename)
            (let ([t (make-code-lookup-table (utf8->string (read-file filename)))])
              (do ([i 0 (fx+ i 1)])
                  ((= i (vector-length t)))
                (execute "insert into line_fps(sfd_fk,fp) values(?,?)" fk (vector-ref t i))))]))
       (execute "select sfd_pk,filename from sfds"))))
    )
