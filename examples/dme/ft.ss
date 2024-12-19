;; SPDX-License-Identifier: MIT
;; Copyright 2024 Indigo BioAutomation, Inc.

#!chezscheme
(library (ft)
  (export ft)
  (import (scheme) (swish erlang) (swish meta) (swish string-utils))

  (define-syntax ft (lambda (x) (syntax-error x "invalid context for")))

  (meta define (->spec x)
    (syntax-case x ()
      [(_ . _) x]
      [field #`[field ,_]]))

  (meta define (context->type ctxt)
    (syntax-case ctxt (quasiquote)
      [`(ft type . _)
       (eq? 'ft (datum ft))
       #'type]))

  (meta define (fld->path fld)
    (datum->syntax #'fld
      (map string->symbol
        (split (symbol->string (syntax->datum fld)) #\.))))

  (define-match-extension ft
    ;; handle-object
    (lambda (v pattern)
      (syntax-case pattern (quasiquote)
        [`(ft type x ...)
         (eq? 'ft (datum ft))
         #`((guard (ftype-pointer? type #,v))
            (handle-fields #,v #,@(map ->spec #'(x ...))))]))
    ;; handle-field
    (lambda (v fld var options context)
      (let ([type (context->type context)]) ;; TODO not sure how fragile this is
        (syntax-case options ()
          [() #`((bind #,var (ftype-ref #,type #,(fld->path fld) #,v)))]
          [else (pretty-syntax-violation "invalid options" context options)]))))

  )

#!eof

> (define-ftype A (struct [x int] [y double]))
> (import (ft))
> (define z (make-ftype-pointer A (foreign-alloc (ftype-sizeof A))))
> (ftype-set! A (x) z 123)
> (ftype-set! A (y) z 3.14)
> (match z [`(ft A ,x ,y) (printf "x: ~s, y: ~3,4f\n" x y)])

> (define-ftype B (struct [p A] [q char]))
> (define g (make-ftype-pointer B (foreign-alloc (ftype-sizeof B))))
> (ftype-set! B (p x) g 404)
> (ftype-set! B (p y) g 7.2)
> (ftype-set! B (q) g (integer->char 128293))
> (match g [`(ft B ,p.x ,p.y) (list p.x p.y)])
