#!chezscheme
(import (scheme))

(let-syntax ([_ (begin ;; run this code at expand time
                  (compile-imported-libraries #t)
                  (undefined-variable-warnings (equal? (getenv "WARN_UNDEFINED") "yes"))
                  ;; (import-notify #t)                                                        
(#%$enable-pass-timing #t)
                  (library-extensions '((".ss" . ".so")))                                  
                  #;   
                  (compile-library-handler                    
                   (lambda (src-path obj-path)
                     (printf "calling compile-library to generate ~a\n" obj-path)  
                     (guard (c [else (printf "EXCEPTION!\n") (display-condition c) (abort)])
                     (compile-library src-path obj-path))))
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

(import (swish imports))
(#%$print-pass-stats)
(base-dir (path-parent (cd)))
(apply swish-start (command-line-arguments))
