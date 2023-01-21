(define-record-type lexical
  (nongenerative)
  (fields
   [mutable references]
   [mutable assignments])
  (protocol
   (lambda (new)
     (lambda ()
       (new '() '())))))

(module ()
  (record-reader 'lexical (record-type-descriptor lexical)))
