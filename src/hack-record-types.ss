(define-record-type lexical-info
  (nongenerative #{lexical-info ble5klpzns025alnatm0ydav9-0})
  (fields
    (immutable name)
    (immutable bind-src)
    (mutable ref-src*)
    (mutable set-src*)))
(define-record-type global-info
  (nongenerative #{global-info ble5klpzns025alnatm0ydav9-1})
  (fields
    (immutable name)
    (mutable ref-src*)
    (mutable set-src*)))
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
   (immutable src) (immutable name) (immutable path) (immutable version) (immutable meta-level) (immutable export*) (immutable import*)
   (immutable export-id*)))




;;------ new stab

(define-record-type source-map
  (nongenerative #{source-map 2lv8mlz2kzyg0qqg338ia10pk-0})
  (fields
   ;; TODO well, darn: fasl-write doesn't like source tables
   ;;                  but we could (fasl-write (source-table-dump st) op)
   (immutable st)         ;; source-table: src -> (source-info ...)
   ;; TODO we could walk the table move nodes whose key is not a symbol into a separate
   ;;      list of nodes that don't need linking beyond the current file
   (immutable key->node)  ;; TODO rename; hashtable mapping {symbol|prelex|local-label} -> source-info
   (immutable prim->node)
   (immutable default-cell)) ;; ?? at any point we're extending source map for at most one ($sfd)
  ;; TODO make this record type opaque / sealed
)

(define-record-type identifier-info
  (nongenerative #{identifier-info nfne4i66hgd1aupfouk6yvuxc-1})
  (fields
   (immutable name)     ;; symbol
   (immutable kind)     ;; prim2 | prim3 | local | global | export | syntax
   ;; TODO these want to tell us about source locations
   ;;  BUT we also want them to be "precise"; i.e., distinguish set! to var from macro call to id w/ same source
   (mutable def)        ;; src
   (mutable set*)       ;; (src ...)
   (mutable ref*))      ;; (src ...)
  (protocol
   (lambda (new)
     (lambda (name kind def-src)
       ;; TODO will we want to replace #f with the magical (make-source-object (#%$sfd) 0 0) ????
       ;;      perhaps in some pass before we resolve everything?
       ;;      heck, maybe we just make a single such source object and stuff it in the source-map itself?
       ;;      then whenever we hit #f in this source map we use that source-object ?
       ;;      (so we do it on demand) ;; OTOH, that might not give us a clean way to drop info related to sfd
       (new name kind def-src '() '())))))

(define-record-type contour-info
  (nongenerative #{contour-info nfne4i66hgd1aupfouk6yvuxc-2})
  (fields
   ;; TODO are name / kind going to be common fields of a parent source-info record type?
   (immutable name)     ;; #f | symbol | library path  ;; TODO what about library version ???
   (immutable kind)     ;; lambda | letrec | letrec* | module | library
   (immutable import*)  ;; (src ...)  ;; TODO more generally: (node ...) ??
   (immutable export*)  ;; (identifier-info ...)
   (immutable bound*)   ;; (identifier-info ...)
   ))

