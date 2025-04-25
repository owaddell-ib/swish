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
                     `((,(cd) . ,(format "~a~cbuild~c~a~clib"
                                 base sep sep which sep)))))
                  (source-directories (map (lambda (x) (if (equal? x ".") (cd) x)) (source-directories)))
                  (include "osi-bootstrap.ss")
                  void)])
  (void))

(include "hack-record-types.ss")

(parameterize ([current-eval interpret] ;; trying to figure out why pass-stats shows compiler active
               ;; TODO maybe we no longer need the following to get top-level ref info?
               ;;   [compile-profile #t] ;; given current hackery for top-level references
               [run-cp0 (lambda (f x) x)])
  (let ([sm (#%$make-source-map)])
    (define (add-source! sx-file) ;; TODO should bake this into expand-to-file
      (let ([ip (open-file-input-port sx-file)])
        (fasl-read ip) ;; recompile info
        (fasl-read ip) ;; #t
        (let ([lsrc (fasl-read ip)])
          (assert (eof-object? (fasl-read ip)))
          (#%$extract-source lsrc sm))
        (close-port ip)))
    ;; TODO temp disable so we can see how long it takes
    (#%$report-source-info
     (case-lambda
      [() sm]
      [(sm) ;; TODO stupid holdover from old interface
       (printf "whee! sc-expand called report-source\n")]))
    (eval '(import (swish imports)))
    ;; TODO THIS IS SO TERRIBLE IN STOCK SCHEME
    (for-each
      (lambda (filename)
        (when (equal? (path-extension filename) "sx")
          (printf "scanning ~s\n" filename)
          ;; TODO BARF BARF
          (add-source! (string-append "../build/release/lib/swish/" filename))))
      (directory-list "../build/release/lib/swish"))
    (printf "*** REMEMBER the .sx scan hack will miss files!\n")    
    ;; Stick with Chez Scheme primitives here (we haven't built Swish yet)
    (let ([filename "/tmp/bolus.fasl"])
      (let ([op (open-file-output-port filename (file-options no-fail #; no-truncate))])
        #; ;; not appending any more
        (file-position op (file-length op))
        (fasl-write
         `#(<sm>
            ,(source-table-dump (source-map-st sm))
            ,(vector-map
              (lambda (cell)
                `(,(car cell)
                  [safe ,(prim-info-safe* (cdr cell))]
                  [unsafe ,(prim-info-unsafe* (cdr cell))]))
              (hashtable-cells
               (source-map-prim->node sm)))
            ;; TODO ooops, this has a mix of identifier-info and eq-hashtables
            ,(hashtable-values (source-map-key->node sm)) ;; TODO retain keys for source-map merge stuff
            ,(source-map-default-cell sm)) ;; TODO remember to deal with this rubbish
         op)
        (close-port op)))))

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
