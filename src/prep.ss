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
    ;; TODO temp disable so we can see how long it takes
    (#%$report-source-info
     (let ([filename "/tmp/source-map.fasl"])
       (define HACK 0)
       (define (dump op what data)
         ;; TODO guessing this might be a convenient format for Chris: read to get category, read to get data
         (fasl-write what op)
         (fasl-write data op))
       (delete-file filename)
       (lambda (outfn
                lexical* global* prim* contour* realm* imports-ht syntax* alias*)
         (let ([op (open-file-output-port filename (file-options no-fail no-truncate))])
           (file-position op (file-length op))
           ;; TODO currently dumping source map each time Scheme calls the report-source-info hook
           ;;      but we could instead build up a list of the results and fasl-write it all at once
           ;;      to make it more compact.
           (dump op 'lexical lexical*)
           (dump op 'global global*)
           (dump op 'prim prim*)
           (dump op 'syntax syntax*)
           (dump op 'contour contour*)
           (dump op 'realm realm*)
           (dump op 'imports-ht imports-ht)
           (dump op 'alias alias*)
           (close-port op)
   
           ;; This is for me to investigate where we're getting extra lexical-info's
           (let ([op (open-file-output-port (format "/tmp/sm-~s.fasl" HACK))])
             (printf "writing output to ~s for ~s\n" (port-name op) outfn)   
             (set! HACK (+ HACK 1))
             (dump op 'lexical lexical*)
             (dump op 'global global*)
             (dump op 'prim prim*)
             (dump op 'syntax syntax*)
             (dump op 'contour contour*)
             (dump op 'realm realm*)
             (dump op 'imports-ht imports-ht)
             (dump op 'alias alias*)
             (close-port op))
   
           ))))
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
