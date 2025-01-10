#!chezscheme
(import (scheme))

(let-syntax ([_ (begin ;; run this code at expand time
                  (compile-imported-libraries #t)
                  ;; (current-eval interpret)
                  (#%$enable-pass-timing #t)
                  ;; (fasl-compressed #f)  
                  (compress-level 'minimum)  
                  (library-extensions '((".ss" . ".sx")))
                  (compile-library-handler expand-library)
                  (putenv "SX" "true")  ;; TODO rm temp hack                
                  (let ([base (path-parent (cd))]
                        [which (if (equal? (getenv "PROFILE_MATS") "yes")
                                   'profile
                                   'release)]
                        [sep (directory-separator)])
                    (library-directories
                     `(("." . ,(format "~a~cbuild~c~a~clib"
                                 base sep sep which sep)))))
                  (include "osi-bootstrap.ss")
                  void)])
  (void))

(include "hack-record-types.ss")

(parameterize ([current-eval interpret] ;; trying to figure out why pass-stats shows compiler active
               ;; TODO maybe we no longer need the following to get top-level ref info?
               ;;   [compile-profile #t] ;; given current hackery for top-level references
               [run-cp0 (lambda (f x) x)])
  (let ([no-src '()])
    (define st (make-source-table))
    (define (log! src x)
      (if src
          (let ([cell (source-table-cell st src '())])
            (set-cdr! cell (cons x (cdr cell))))
          (set! no-src (cons x no-src))))
    (define (find! src category)
      (if (not src)
          category
          (let ([cell (source-table-cell st src category)])
            (assert (eq? category (cdr cell)))
            cell)))
    (#%$report-source-info
     (case-lambda
      [(context src prelex-src)
       ;; ref, set!
       (let ([elt (find! src context)])
         (if (not prelex-src)
             ;; TODO could commonize no-src elts
             (set! no-src (cons elt no-src))
             (let* ([cell (source-table-cell st prelex-src #f)]
                    [info (or (cdr cell)
                              (let ([info (make-lexical)])
                                (set-cdr! cell info)
                                info))])
               (unless (lexical? info)
                 (errorf #f "expected lexical, but got ~s for src=~s prelex-src=~s"
                   info src prelex-src))  
               (case context
                 [(ref)
                  (lexical-references-set! info
                    (cons elt (lexical-references info)))]
                 [(set!)
                  (lexical-assignments-set! info
                    (cons elt (lexical-assignments info)))]
                 [else (errorf 'report-source-info "unexpected context ~s" context)]))))]
       ;;
       ;;                                                                       
       ;;  TODO LEFT OFF HERE
       ;;  --> instead we should be creating def-use chain here
       ;;      maybe we have a struct with:
       ;;      source-table mapping src -> info-about-binding
       ;;        then info-about-binding has:
       ;;           refs
       ;;           sets
       ;;        and we store some kind of symbolic ref to those
       ;;        or a graph ref to the corresponding source-table cell?
       ;;                                                                       
       ;;
       ;; BUG  ponder confusing sourcerer output, e.g., we get a (context src prelex-src)
       ;;      where context is 'ref and *both* src and prelex-src point to the same thing:
       ;;      #<source swish/osi.ss[5063:5069]>
       ;;       --> probably because the macro is taking a single piece of source and
       ;;           plunking it down both as lambda formals and as reference to that formal
       ;;           but the macro uses the same identifier both times, so they have the same
       ;;           source and sourcerer doesn't (yet?) have a notion of the prelex that we
       ;;           need in order to distinguish them
       ;;       --> is there some way we can link the macro-argument source expression
       ;;           with the source on the pattern variable within the macro?
       ;;               
       ;;  TODO YET ANOTHER IDEA
       ;;    - what if we had separate source-tables?
       ;;       - one for prelexes
       ;;         - this could be our structure that records lists of refs and sets
       ;;           where each element in those lists is a token
       ;;       - one for refs and sets
       ;;         - this could be a map from source -> token
       ;;         - we'd have to invert the mapping when we load the source table
       ;;           so we have token -> source and then we can use tokens within the prelex
       ;;           table
       ;;
      [(context src x2 x3)
       (void)
       #;                               
       ;; primref, primset!, tl-ref, tl-set!, lambda, letrec, letrec*
       (log! src (vector src x2 x3))]))
    (eval '(import (swish imports)))
    ;; Stick with Chez Scheme primitives here (we haven't built Swish yet)
    (let* ([filename "report-source-info-output.source-table"]
           [!! (delete-file filename)]
           [op (open-output-file filename)])
      (printf "~s entries w/o src\n" (length no-src))
      (dynamic-wind void
        (lambda ()
          (put-source-table op st)
          (fprintf op "\n#!eof\n")
          (pretty-print no-src op))
        (lambda () (close-port op))))))

(#%$print-pass-stats)

#!eof

* this works (remember to clean build dir first)
   0. rm build/release/lib/swish/* build/release/bin/*.library
   1. cd src
   2. ./prep
   3. cd ..
   4. make

* this also works
   0. rm build/release/lib/swish/* build/release/bin/*.library
   1. cd src
   2. ./prep
   2. ./go
   3. cd ..
   4. make

* BUT if you forget to clean the swish-core.library, you'll get an error message:

make -C src/swish all
swish-core.library is up to date
compiling swish/events.ss
 looking for ../build/release/lib/swish/events.sx
attempting to use ../build/release/lib/swish/events.sx
Exception: compiled (swish events) requires a different compilation instance of (swish meta) from the one previously loaded from ../build/release/bin/swish-core.library
