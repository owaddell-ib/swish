
(include "hack-record-types.ss")

(library (hack)
  (export
   add-globals!
   add-lexicals!
   edge
   graph
   make-graph
   node
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

  (define-record-type node (nongenerative) (fields name type src))
  (define-record-type edge (nongenerative) (fields type from to))
  (define-record-type graph
    (nongenerative)
    (fields nodes in-edges out-edges)
    (protocol
     (lambda (new)
       (lambda ()
         (new
          (make-hashtable symbol-hash eq?)
          (make-eq-hashtable)
          (make-eq-hashtable))))))

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
      (let ([cell (if bind-src
                      (source-table-cell dedup bind-src #f)
                      (hashtable-cell anon name #f))])
        (when (cdr cell) (printf ";; duplicate ~s binding for ~s at ~s\n" type name bind-src))
        (or (cdr cell)
            (let ([n (add-node! name (make-node name type bind-src))])
              (set-cdr! cell n)
              n))))
    get-node)

  (define (add-lexicals! g lex-info-v)
    (with-graph g
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

  (define (add-globals! g global-info-v)
    (with-graph g
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

  )

(import (hack))
(define g (make-graph))

(define lexical-db (make-hashtable symbol-hash eq?))
(define global-db (make-hashtable symbol-hash eq?))
(define imports-db (make-hashtable symbol-hash eq?))
(define realm-db (make-hashtable symbol-hash eq?))
(define library-db (make-hashtable equal-hash equal?))
(define syntax-db (make-hashtable symbol-hash eq?))
(define prim-db (make-hashtable symbol-hash equal?))
(define *alias* '())
(define *contour* '())
(define whence-db (make-eq-hashtable))

(define (whence! obj filename)
  (hashtable-update! whence-db obj (lambda (prev) (cons obj prev)) '()))

(define (whence obj)
  (hashtable-ref whence-db obj '()))

(define (smash-lexical! filename liv)
  (add-lexicals! g liv)
  (vector-for-each
   (lambda (li)
     (hashtable-update! lexical-db (lexical-info-name li)
       (lambda (prev)
         (whence! li filename)
         (cons li prev))
       '()))
   liv))

(define (smash-global! filename giv)
  (add-globals! g giv)
  (vector-for-each
   (lambda (gi)
     (hashtable-update! global-db (global-info-name gi)
       (lambda (prev)
         (whence! gi filename)
         (cons gi prev))
       '()))
   giv))

(define (smash-imports! filename import-ht)
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
   (lambda (si)
     (hashtable-update! syntax-db (syntax-info-name si)
       (lambda (prev)
         (whence! si filename)
         (cons si prev))
       '()))
   siv))

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

(define (sm) (slurp "/tmp/source-map.fasl"))
(define (sm*)
  (fold-files "/tmp" #f (lambda (dir) #f)
    (lambda (filename _)
      (when (pregexp-match-positions (re ".*/sm-.*\\.fasl") filename)
        (printf "slurp: ~a\n" filename)
        (slurp filename)))))

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
