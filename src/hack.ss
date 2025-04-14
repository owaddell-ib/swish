
(include "hack-record-types.ss")

(library (hack)
  (export
   add-globals!
   add-imports!
   add-lexicals!
   add-realms!
   add-syntax!
   edge
   foreach          ;;; HACK  should come from elsewhere
   graph
   hashtable-add!   ;;; HACK tired of writing this confounded thing
   make-tome
   node
   tome
   )
  (import (scheme) (swish imports))
  (include "hack-record-types.ss")
  (define-syntax foreach
    (syntax-rules ()
      [(_ ([var collection*] ...) e0 e1 ...)
       (let ([f (lambda (var ...) e0 e1 ...)]
             [var collection*] ...)
         (cond
          [(and (vector? var) ...) (vector-for-each f var ...)]
          [else (for-each f var ...)]))]))

  (define-record-type node (nongenerative) (fields name type (mutable src)))
  (define-record-type edge (nongenerative) (fields type from to))
  (define-record-type graph
    (nongenerative)
    (fields nodes in-edges out-edges)
    (protocol
     (lambda (new)
       (lambda (node-ht)
         (new node-ht (make-eq-hashtable) (make-eq-hashtable))))))
  (define-record-type tome
    (nongenerative)
    (fields realms ids)
    (protocol
     (lambda (new)
       (lambda ()
         (new
          (make-graph (make-hashtable symbol-hash eq?))
          (make-graph (make-hashtable symbol-hash eq?)))))))

  (define (hashtable-add! ht key elt)
    (hashtable-update! ht key
      (lambda (prev) (cons elt prev))
      '()))

  (define (with-graph g k)
    (match-define `(graph ,nodes ,in-edges ,out-edges) g)
    (define (add-node! name node)
      (hashtable-add! nodes name node)
      node)
    (define (add-edge! type from to)
      (let ([edge (make-edge type from to)])
        (hashtable-add! out-edges from edge)
        (hashtable-add! in-edges to edge)
        edge))
    (k add-node! add-edge!))

  ;; BARF
  (define (make-get-node type add-node!)
    (define dedup (make-source-table))
    (define anon (make-hashtable symbol-hash eq?))
    (define (get-node name bind-src)
      (let ([cell (if (source-object? bind-src)
                      (source-table-cell dedup bind-src #f)
                      (hashtable-cell anon name #f))])
        (when (cdr cell) (printf ";; duplicate ~s binding for ~s at ~s\n" type name bind-src))
        (or (cdr cell)
            (let ([n (add-node! name (make-node name type bind-src))])
              (set-cdr! cell n)
              n))))
    get-node)

  (define (add-lexicals! T lex-info-v)
    (with-graph (tome-ids T)
      (lambda (add-node! add-edge!)
        (define get-node (make-get-node 'lexical add-node!))
        (foreach ([li lex-info-v])
          (match-let*
           ([`(lexical-info ,name ,bind-src ,ref-src* ,set-src*) li]
            [,binding (get-node name bind-src)])
           (foreach ([ref-src ref-src*])
             (add-edge! 'ref (get-node name ref-src) binding))
           (foreach ([set-src set-src*])
             (add-edge! 'set (get-node name set-src) binding)))))))

  (define (add-globals! T global-info-v)
    (with-graph (tome-ids T)
      (lambda (add-node! add-edge!)
        (define get-node (make-get-node 'global add-node!))
        (foreach ([gi global-info-v])
          (match-let*
           ([`(global-info ,name ,ref-src* ,set-src*) gi]
            [,binding (get-node name
                        (match set-src*
                          [(,set-src) set-src]
                          [,_ #f]))])
           (foreach ([ref-src ref-src*])
             (add-edge! 'ref (get-node name ref-src) binding))
           (foreach ([set-src set-src*])
             (add-edge! 'set (get-node name set-src) binding)))))))

  (define (add-syntax! T syntax-info-v)
    (with-graph (tome-ids T)
      (lambda (add-node! add-edge!)
        (define get-node (make-get-node 'syntax add-node!))
        (foreach ([si syntax-info-v])
          (match-let*
           ([`(syntax-info ,name ,bind-src ,ref-src*) si] ;; TODO src-src* if we handle fluid-let-syntax
            [,binding (get-node name bind-src)])
           (foreach ([ref-src ref-src*])
             (add-edge! 'ref (get-node name ref-src) binding)))))))

  (module HACK_BARF (dig-for-source)
    (define (dig-for-source x)
      (cond
       [(annotation? x) (annotation-source x)]
       [(identifier? x) (dig-for-source (syntax-object-expression x))]
       [else #f]))
    ;; ripped off from meta.ss
    (define so-rtd (record-rtd #'_))
    (define syntax-object? (record-predicate so-rtd))
    (define make-syntax-object (record-constructor so-rtd))
    (define flds (record-type-field-names so-rtd))
    (define (make-accessor fld-name)
      (let ([i (ormap (lambda (f i) (and (eq? f fld-name) i))
                 (vector->list flds)
                 (iota (vector-length flds)))])
        (record-accessor so-rtd i)))
    (define syntax-object-expression (make-accessor 'expression)))

  (define (add-realms! T realm*)
    (import HACK_BARF)
    (with-graph (tome-realms T)
      (lambda (add-module-node! add-module-edge!)
        ;; TODO determine realm type by looking at realm-path
        (define get-module-node (make-get-node 'realm add-module-node!))
        (define (HACK-lookup-module-node name)
          (hashtable-ref (graph-nodes (tome-realms T)) name #f))
        (define (HACK-get-module-node name)
          (or (HACK-lookup-module-node name)
              (begin
                (printf "Dang. Import of realm ~s before we processed its realm\n" name)
                (get-module-node name #f))))
        (with-graph (tome-ids T)
          (lambda (add-node! add-edge!)
            (define get-export (make-get-node 'global add-node!))
            (define (HACK-get-export export-id)
              (or (hashtable-ref (graph-nodes (tome-ids T)) export-id #f)
                  (begin
                    (printf "processed realm export ~s before references to it\n" export-id)
                    #f)))
            (foreach ([ri realm*])
              (match-let*
               ([`(realm ,src ,name ,path ,version ,meta-level ,export* ,import* ,export-id*) ri])
               ;; TODO we should borrow Chris's notion of adding properties to nodes so we could record this stuff
               ;;      - OTOH, nodes / edges might become the new representation if we can figure out what it should
               ;;        look like by experimenting here
               ;; TODO dropping perfectly good import* information on the floor
               ;;      add-imports! will add the source links showing where we were imported
               ;;      but the import* here show our connections to other libraries
               ;;       - in theory we might be able to figure out which of the sources
               ;;         we process in add-imports! exist within the bfp efp of the
               ;;         library whose node we find by lookup up an id in import*
               ;;       - but, I suspect we care about these links more for internal
               ;;         stuff where server might show an import graph or something
               (cond
                [(not name)
                 ;; TODO currently happens for things like define-enumeration
                 (printf "no name for realm: src=~s path=~s export*=~s\n" src path export*)]
                [(HACK-lookup-module-node name) =>
                 (lambda (hits)
                   (unless (= 1 (length hits)) (printf "processing realm ~s and found more than one node representing it: ~s\n" name hits))
                   (foreach ([hit hits])
                     (node-src-set! hit src)))]
                [else (get-module-node name src)])
               ;; patch up the export-ids: find the node with no source and install the source we have
               ;; TODO maybe sourcerer should be resolving the source for export-id* for us:
               ;;      just give mapping of ((export-id . src) ...)
               ;;      where src is the binding source for the lexical binding whose value we export
               (foreach ([export-id export-id*])
                 (match-let* ([(,exported . ,id) export-id]
                              [,src (dig-for-source id)]) ;; TODO see above
                   (unless src (printf "no source for ~s export ~s on ~s\n" name exported id))
                   (when src
                     (cond
                      [(HACK-get-export exported) =>
                       (lambda (export-node*)
                         (printf "---\ntry to install source for export: ~s\n " exported)
                         (printf "  found source ~s\n" src)
                         (let ([missing-source* (filter (lambda (n) (not (node-src n))) export-node*)])
                           (unless (= 1 (length missing-source*))
                             ;; TODO if we hit this, maybe it's just that we've processed realm info for the same library multiple times
                             ;;      for example, a library that we need at compile time and at run time
                             (printf "Rats: we should have a single global assignment with no source (from build-library-body)\n")
                             (printf "      but instead we have these:~{  ~s\n~}\n" missing-source*))
                           (foreach ([export-node missing-source*])
                             (printf "  existing node ~s\n" export-node)
                             (node-src-set! export-node src))))]
                      [else
                       (printf "---\ninstall new export ~s with source ~s\n" exported src)
                       (get-export exported src)])))))))))))

  (define (add-imports! T import*)
    (with-graph (tome-realms T)
      (lambda (add-module-node! add-module-edge!)
        ;; TODO see note in add-realms!
        (define get-module-node (make-get-node 'realm add-module-node!))
        (define (HACK-lookup-module-node name)
          (hashtable-ref (graph-nodes (tome-realms T)) name #f))
        (foreach ([id.src* import*])
          (match-let*
           ([(,id . ,src*) id.src*]
            [,binding
             (cond
              [(HACK-lookup-module-node id) =>
               (lambda (hits)
                 (unless (= 1 (length hits))
                   (printf "Uh, surprised to have found more than one node representing realm ~s: ~s\n" id hits))
                 (car hits))]
              [else
               (printf "processing import src* for ~s before processing its realm\n" id)
               ;; ... so we'll have to wire in its source later in add-realms!
               (get-module-node id #f)])])
           (foreach ([src src*])
             (add-module-edge! 'import src binding)))))))

  )

(import (hack))
(define T (make-tome))

(define lexical-db (make-hashtable symbol-hash eq?))
(define global-db (make-hashtable symbol-hash eq?))
(define imports-db (make-hashtable symbol-hash eq?))
(define realm-db (make-hashtable symbol-hash eq?))
(define library-db (make-hashtable equal-hash equal?))
(define syntax-db (make-hashtable symbol-hash eq?))
(define syntax-raw-before-rewiring (make-hashtable symbol-hash eq?))
(define prim-db (make-hashtable symbol-hash equal?))
(define *alias* '())
(define *contour* '())
(define whence-db (make-eq-hashtable))

(define (whence! obj filename)
  (hashtable-update! whence-db obj (lambda (prev) (cons obj prev)) '()))

(define (whence obj)
  (hashtable-ref whence-db obj '()))

(define (smash-lexical! filename liv)
  (add-lexicals! T liv)
  (vector-for-each
   (lambda (li)
     (hashtable-update! lexical-db (lexical-info-name li)
       (lambda (prev)
         (whence! li filename)
         (cons li prev))
       '()))
   liv))

(define (smash-global! filename giv)
  (add-globals! T giv)
  (vector-for-each
   (lambda (gi)
     (hashtable-update! global-db (global-info-name gi)
       (lambda (prev)
         (whence! gi filename)
         (cons gi prev))
       '()))
   giv))

(define (smash-imports! filename import-ht)
  (add-imports! T (hashtable-cells import-ht))
  (vector-for-each
   (lambda (cell)
     (match-define (,key . ,src*) cell)
     (hashtable-update! imports-db key
       (lambda (prev)
         (whence! src* filename)
         (append src* prev))
       '()))
   (hashtable-cells import-ht)))

(define (smash-realms! filename realm*)
  (add-realms! T realm*)
  (for-each
   (lambda (r)
     (match-define `(realm ,name ,path) r)
     (if (not (symbol? name))
         (printf "Whoa: name is ~s for ~s\n" name r)
         (hashtable-update! realm-db name
           (lambda (prev)
             (whence! r filename)
             (cons r prev))
           '()))
     (hashtable-update! library-db path
       (lambda (prev) (cons r prev))
       '()))
   realm*))

(define (smash-syntax! filename siv)
  (vector-for-each
   (lambda (x)
     (define-values (key val)
       (match x
         [(,key . ,val) (values key val)]
         [`(syntax-info ,name) (values name x)]))
     ;; TODO not thrilled about s/syntax.ss having to exposing the raw label (car cell) for us here
     ;;      but we need that to link things together in link-syntax! later
     ;; TODO maybe Chez Scheme will end up exposing some kind of extend-source-map! or some such
     ;;      that takes stuff and magically "merges" it with another source map to resolve those links
     (hashtable-update! syntax-raw-before-rewiring key
       (lambda (prev) (cons val prev))
       '()))
   siv))

;; post-pass to consolidate syntax-infos that had the same label over in syntax.ss
(define (link-syntax!)
  (define sinfos '())
  (foreach ([cell (hashtable-cells syntax-raw-before-rewiring)])
    (match-define (,label . ,nodes) cell)
    (define levels (make-hashtable values fx=))
    (foreach ([node nodes])
      (hashtable-add! levels (syntax-info-meta-level node) node))
    (foreach ([level.group (hashtable-cells levels)])
      (match-define (,level . ,group) level.group)
      (let gather ([group group] [name #f] [bind-src #f] [ref-src* '()])
        (match group
          [() (set! sinfos (cons (make-syntax-info name bind-src level ref-src*) sinfos))]
          [(,si . ,group)
           (let ([name (or name (syntax-info-name si))]
                 [bind-src (or bind-src
                               (let ([src (syntax-info-bind-src si)])
                                 (cond
                                  [(gensym? src)
                                   (printf "in theory we could use ~s to find source\n" src)
                                   ;; instead let the loop find it
                                   #f]
                                  [else src])))])
             (gather group name bind-src
               (append (syntax-info-ref-src* si) ref-src*)))]))))
  (add-syntax! T sinfos)
  (for-each
   (lambda (si)
     (hashtable-update! syntax-db (syntax-info-name si)
       (lambda (prev)
;;;         (whence! si filename)
         (cons si prev))
       '()))
   sinfos))

(define (smash-prim! filename piv)
  (vector-for-each
   (lambda (pi)
     (hashtable-update! prim-db (prim-info-name pi)
       (lambda (prev)
         (whence! pi filename)
         (cons pi prev))
       '()))
   piv))

(define (slurp filename)
  (define ip (open-binary-file-to-read filename))
  (on-exit (close-port ip)
    (let go ()
      (match (fasl-read ip)
        [lexical (smash-lexical! filename (fasl-read ip)) (go)]
        [global (smash-global! filename (fasl-read ip)) (go)]
        [imports-ht (smash-imports! filename (fasl-read ip)) (go)]
        [realm (smash-realms! filename (fasl-read ip)) (go)]
        [alias (set! *alias* (append (fasl-read ip) *alias*)) (go)]
        [contour (set! *contour* (append (fasl-read ip) *contour*)) (go)]
        [syntax (smash-syntax! filename (fasl-read ip)) (go)]
        [prim (smash-prim! filename (fasl-read ip)) (go)]
        [#!eof (void)]
        [,other (printf "IGNORING ~s\n" other) (fasl-read ip) (go)]))))

(define (sm) (slurp "/tmp/source-map.fasl") (link-syntax!))
(define (sm*)
  (fold-files "/tmp" #f (lambda (dir) #f)
    (lambda (filename _)
      (when (pregexp-match-positions (re ".*/sm-.*\\.fasl") filename)
        (printf "slurp: ~a\n" filename)
        (slurp filename))))
  (link-syntax!))

;; returns result of $extract-source, which
;; currently returns multiple values:
;;   1. a source-table mapping source location to symbol: call | case-lambda
;;   2. a vector of lexical-info structures harvested from the file
;;   3. a vector of global-info structures harvested from the file
(define (get-source sx-file)
  (let ([ip (open-binary-file-to-read sx-file)])
    (on-exit (close-port ip)
      (fasl-read ip) ;; recompile info
      (fasl-read ip) ;; #t
      (let ([lsrc (fasl-read ip)])
        (assert (eof-object? (fasl-read ip)))
        (#%$extract-source lsrc)))))


(printf ";;  Example:
;;   > (sm*)  ;; or (sm)
;;   > (define-values (st locals globals) (get-source \"../build/release/lib/swish/cli.sx\"))\n")

(define (show id)
  (cond
   [(hashtable-ref lexical-db id #f) => inspect]
   [(hashtable-ref global-db id #f) => inspect]))

(define (show-imports id)
  (inspect (hashtable-ref imports-db id '())))

(define (show-library path)
  (inspect (hashtable-ref library-db path '())))

;; somewhat unwieldy example of using some of the data we have
(define (imports path)
  (cond
   [(hashtable-ref library-db path #f) =>
    (lambda (realm*)
      (for-each
       (lambda (r)
         (cond
          [(realm? r)
           (printf "realm ~s is ~s\n" (realm-name r) (realm-path r))
           (printf "  imported at these locations:~{\n    ~s~}\n"
             (hashtable-ref imports-db (realm-name r) '()))
           (for-each
            (lambda (id)
              (printf "  imports ~s with internal name ~s\n"
                (cond
                 [(hashtable-ref realm-db id #f) =>
                  (lambda (r*)
                    (match r*
                      [(,r . ,_)
                       ;; TODO for now take the first one; we should merge the info on import
                       (realm-path r)]
                      [,_ "dunno"]))]
                 [else "a core library"])
                id))
            (realm-import* r))]
          [else (printf "not a realm, just: ~s\n" r)]))
       realm*))]
   [else (printf "found nothing for ~s\n" path)]))




#!eof

(match-define `(tome ,realms ,ids) T)
(match-define `(graph [nodes ,r-nodes] [in-edges ,r-in-edges] [out-edges ,r-out-edges]) realms)
(match-define `(graph [nodes ,id-nodes] [in-edges ,id-in-edges] [out-edges ,id-out-edges]) ids)

(define info
  (let ([rename (make-hashtable string-hash string=?)])
    (define (raw-info id)
      (define nodes (hashtable-ref id-nodes id '()))
      (define root (find (lambda (N) (not (hashtable-ref id-out-edges N #f))) nodes))
      (match root
        [`(node ,name ,type ,src)
         (printf "~s ~s bound at ~s\n" name type src)
         (printf " references:\n~:{   ~s ~s\n~}"
           (map
            (lambda (e)
              (match e
                [`(edge ,type [from `(node ,src)]) (list type src)]
                [,_ (printf "NOT AN EDGE: ~s\n" e)]))
            (hashtable-ref id-in-edges root '())))]
        [,_ (printf "while looking for ~s: didn't find a root node (among ~s nodes)\n" id (length nodes))])
      (newline))
    ;; translate raw names so we can lookup throw instead of #{throw m28beaodm9yu0orlbadpwg2cr-723}
    (vector-for-each
     (lambda (name)
       (hashtable-add! rename (symbol->string name) name))
     (hashtable-keys id-nodes))
    (lambda (id)
      (let ([cooked (hashtable-ref rename (symbol->string id) '())])
        (raw-info id)
        (for-each raw-info (remq id cooked))))))

;; doesn't do well yet with:
;;  (info 'throw)    ;; doesn't link up the bind-src
