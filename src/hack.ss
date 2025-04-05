
(include "hack-record-types.ss")

(define lexical-db (make-hashtable symbol-hash eq?))
(define global-db (make-hashtable symbol-hash eq?))
(define imports-db (make-hashtable symbol-hash eq?))
(define realm-db (make-hashtable symbol-hash eq?))
(define library-db (make-hashtable equal-hash equal?))
(define *alias* '())
(define whence-db (make-eq-hashtable))

(define (whence! obj filename)
  (hashtable-update! whence-db obj (lambda (prev) (cons obj prev)) '()))

(define (whence obj)
  (hashtable-ref whence-db obj '()))

(define (smash-lexical! filename liv)
  (vector-for-each
   (lambda (li)
     (hashtable-update! lexical-db (lexical-info-name li)
       (lambda (prev)
         (whence! li filename)
         (cons li prev))
       '()))
   liv))

(define (smash-global! filename giv)
  (vector-for-each
   (lambda (gi)
     (hashtable-update! global-db (global-info-name gi)
       (lambda (prev)
         (whence! gi filename)
         (cons gi prev))
       '()))
   giv))

(define (smash-imports! filename import-ht)
  (vector-for-each
   (lambda (cell)
     (match-define (,key . ,src*) cell)
     (hashtable-update! imports-db key
       (lambda (prev)
         (whence! src* filename)
         (append src* prev))
       '()))
   (hashtable-cells import-ht)))

(define (smash-realms! filename realm*)
  (for-each
   (lambda (r)
     (match-define `(realm ,name ,path) r)
     (if (not (symbol? name))
         (printf "Whoa: name is ~s for ~s\n" name r)
         (hashtable-update! realm-db name
           (lambda (prev)
             (whence! r filename)
             (cons r prev))
           '()))
     (hashtable-update! library-db path
       (lambda (prev) (cons r prev))
       '()))
   realm*))

(define (slurp filename)
  (define ip (open-binary-file-to-read filename))
  (on-exit (close-port ip)
    (let go ()
      (match (fasl-read ip)
        [lexical (smash-lexical! filename (fasl-read ip)) (go)]
        [global (smash-global! filename (fasl-read ip)) (go)]
        [imports-ht (smash-imports! filename (fasl-read ip)) (go)]
        [realm (smash-realms! filename (fasl-read ip)) (go)]
        [alias (set! *alias* (append (fasl-read ip) *alias*)) (go)]
        [#!eof (void)]
        [,other (printf "IGNORING ~s\n" other) (fasl-read ip) (go)]))))

(define (sm) (slurp "/tmp/source-map.fasl"))
(define (sm*)
  (fold-files "/tmp" #f (lambda (dir) #f)
    (lambda (filename _)
      (when (pregexp-match-positions (re ".*/sm-.*\\.fasl") filename)
        (printf "slurp: ~a\n" filename)
        (slurp filename)))))

(define (show id)
  (cond
   [(hashtable-ref lexical-db id #f) => inspect]
   [(hashtable-ref global-db id #f) => inspect]))

(define (show-imports id)
  (inspect (hashtable-ref imports-db id '())))

(define (show-library path)
  (inspect (hashtable-ref library-db path '())))

;; somewhat unwieldy example of using some of the data we have
(define (imports path)
  (cond
   [(hashtable-ref library-db path #f) =>
    (lambda (realm*)
      (for-each
       (lambda (r)
         (cond
          [(realm? r)
           (printf "realm ~s is ~s\n" (realm-name r) (realm-path r))
           (printf "  imported at these locations:~{\n    ~s~}\n"
             (hashtable-ref imports-db (realm-name r) '()))
           (for-each
            (lambda (id)
              (printf "  imports ~s with internal name ~s\n"
                (cond
                 [(hashtable-ref realm-db id #f) =>
                  (lambda (r*)
                    (match r*
                      [(,r . ,_)
                       ;; TODO for now take the first one; we should merge the info on import
                       (realm-path r)]
                      [,_ "dunno"]))]
                 [else "a core library"])
                id))
            (realm-import* r))]
          [else (printf "not a realm, just: ~s\n" r)]))
       realm*))]
   [else (printf "found nothing for ~s\n" path)]))
