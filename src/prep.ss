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
    ;; takes 5.23 sec if we do it this way and generates a 17Mb file
    ;; (#%$hack-log-id (lambda x (fasl-write x op)))
    ;; by contrast, we get a file of 500Kb and it takes just 2.1 sec if we accumulate
    ;; and write it at the end so we get the benefit of fasl commonization for the
    ;; various source annotations, I reckon
    (define ls '())
    (#%$hack-log-id (lambda x (set! ls (cons x ls))))
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
