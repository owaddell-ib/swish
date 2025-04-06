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
