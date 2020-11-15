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

(parameterize ([current-eval interpret] ;; trying to figure out why pass-stats shows compiler active
               ;; TODO maybe we no longer need the following to get top-level ref info?
               ;;   [compile-profile #t] ;; given current hackery for top-level references
               [run-cp0 (lambda (f x) x)])
  (let* ([filename "hack-log-id-output.fasl"]
         [_ (delete-file filename)] ;; can't remember file-options stuff for open-file-output-port
         [op (open-file-output-port filename)])
    ;; generates a much smaller file if we accumulate the results and do a
    ;; single fasl-write vs. multiple fasl-writes since the former commonizes
    ;; source objects
    (define ls '())
    (define (log! x) (set! ls (cons x ls)))
    (#%$hack-log-id
     (case-lambda
      [(x0 x1 x2)
       ;; ref, set!
       (log! (vector x0 x1 x2))]
      [(x0 x1 x2 x3)
       ;; primref, primset!, tl-ref, tl-set!, lambda, letrec, letrec*
       (log! (vector x0 x1 x2 x3))]))
    (eval '(import (swish imports)))
    (fasl-write (reverse ls) op)
    ))

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
