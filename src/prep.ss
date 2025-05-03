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

;; TODO this is stuff we need to move into Chez Scheme; but it's easier to iterate here
(define (link-source-map! sm)
  ;; TODO some sort of link phase; likely on-demand, when we merge source-maps, etc.
  ;;    - wire interface-info-impreq* to the corresponding interface-info node
  ;;    - wire interface-info-export* to the corresponding identifier-info
  (let ([key->node (source-map-key->node sm)])

    (define (resolve resolved?)
      (lambda (key)
        (if (resolved? key)
            key ;; resolved on hypothetical earlier link of source-map
            (hashtable-ref key->node key key))))

    (vector-for-each
     (lambda (cell)
       (let ([key (car cell)] [node (cdr cell)])
           ;; TODO ? may want to put interfaces (and other stuff that needs linking) into a separate key->node map
           ;;      so we can find it faster (i.e., if we don't need to link other node types)
         (cond
          [(interface-info? node)
           ;; Link imports to the interface-info they imported if we can resolve it.
           ;; TODO the Chez Scheme version may need some way to dump results for us that filters out the key; maybe replace with #f so we know there's something unresolved?
           (interface-info-impreq*-set! node
             (map (resolve interface-info?)
               (interface-info-impreq* node)))
           ;; Link exports to their identifier-info nodes
           (interface-info-export*-set! node
             (map (resolve identifier-info?)
               (interface-info-export* node)))])))
     (hashtable-cells key->node))

    ))

(parameterize ([current-eval interpret] ;; trying to figure out why pass-stats shows compiler active
               ;; TODO maybe we no longer need the following to get top-level ref info?
               ;;   [compile-profile #t] ;; given current hackery for top-level references
               [run-cp0 (lambda (f x) x)])
  (let ([sm (#%$make-source-map)])
    #;           
    ;; TODO temp disable so we can see how long it takes
    (#%$report-source-info
     (case-lambda
      [() sm]
      [(sm) ;; TODO stupid holdover from old interface
       (printf "whee! sc-expand called report-source\n")]))
    (parameterize ([#%$current-source-map sm])
      (eval '(import (swish imports))))
    ;; Do the linking that Chez Scheme needs to do for us eventually:
    (link-source-map! sm)
    ;; Stick with Chez Scheme primitives here (we haven't built Swish yet)
    (let ([filename "/tmp/bolus.fasl"])
      (let ([op (open-file-output-port filename (file-options no-fail))])
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
