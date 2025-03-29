
(include "hack-record-types.ss")

(define lexical-db (make-hashtable symbol-hash eq?))
(define global-db (make-hashtable symbol-hash eq?))
(define imports-db (make-hashtable symbol-hash eq?))
(define realm-db (make-hashtable symbol-hash eq?))
(define library-db (make-hashtable equal-hash equal?))

(define (smash-lexical! filename liv)
  (vector-for-each
   (lambda (li)
     (hashtable-update! lexical-db (lexical-info-name li)
       (lambda (prev) (cons (cons filename li) prev))
       '()))
   liv))

(define (smash-global! filename giv)
  (vector-for-each
   (lambda (gi)
     (hashtable-update! global-db (global-info-name gi)
       (lambda (prev) (cons (cons filename gi) prev))
       '()))
   giv))

(define (smash-imports! filename import-ht)
  (vector-for-each
   (lambda (cell)
     (match-define (,key . ,src*) cell)
     (hashtable-update! imports-db key
       (lambda (prev) (cons (cons filename src*) prev))
       '()))
   (hashtable-cells import-ht)))

(define (smash-realms! filename realm*)
  (for-each
   (lambda (r)
     (match-define `(realm ,name ,path) r)
     (if (not (symbol? name))
         (printf "Whoa: name is ~s for ~s\n" name r)
         (hashtable-update! realm-db name
           (lambda (prev) (cons (cons filename r) prev))
           '()))
     (hashtable-update! library-db path
       (lambda (prev) (cons (cons filename r) prev))
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
        [#!eof (void)]
        [,other (printf "IGNORING ~s\n" other) (fasl-read ip) (go)]))))

(define (show id)
  (cond
   [(hashtable-ref lexical-db id #f) => inspect]
   [(hashtable-ref global-db id #f) => inspect]))

(define (show-imports id)
  (inspect (hashtable-ref imports-db id '())))

(define (show-library path)
  (inspect (hashtable-ref library-db path '())))
