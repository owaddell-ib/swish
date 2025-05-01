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

(define-record-type prim-info
  (nongenerative #{prim-info hnfnegd1aupfouk6yvuxc466i-2})
  (fields
   (immutable name)     ;; symbol
   (mutable safe*)      ;; (src ...)
   (mutable unsafe*))   ;; (src ...)
  (protocol
   (lambda (new)
     (lambda (name)
       (new name '() '())))))

;; TODO instead of making the fields mutable, should we have an intermediate record type for cases like
;;      add-import! that have only partial information and then merge that info when we get it?
(define-record-type contour-info
  (nongenerative #{contour-info j1qn4yon710auwsxmmkymfryk-0})
  (fields
   ;; TODO are name / kind going to be common fields of a parent source-info record type?
   ;;      - not sure this has a name
   (mutable kind)     ;; lambda | letrec | letrec* | module | library
   (mutable src)      ;; bfp/efp for region: e.g., individual case-lambda clause
   ;; TODO should we move the parameterize inside the build-letrec* in build-library-body so it can record contour for us?
   ;;      i.e., maybe contour should be just src and bound* and src so we avoid the mess of name, export*, and ref*
   (mutable bound*)   ;; (identifier-info ...)
   ))

(define-record-type interface-info
  (nongenerative #{interface-info j1qn4yon710auwsxmmkymfryk-1})
  (fields
   ;; TODO are name / kind going to be common fields of a parent source-info record type?
   (mutable name)     ;; #f | symbol | library path  ;; TODO what about library version ???
   (mutable kind)     ;; lambda | letrec | letrec* | module | library
   (mutable src)      ;; bfp/efp for region: e.g., individual case-lambda clause
   (mutable ref*)     ;; edges showing where we were imported (src ...)  ;; TODO more generally: (node ...) ??
   (mutable impreq*)  ;; (uid ...)
   (mutable export*)  ;; #(label ...)
   ))
