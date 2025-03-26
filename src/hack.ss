
(include "hack-record-types.ss")

(define db (make-hashtable symbol-hash eq?))

(define (smash-lexical! liv)
  (vector-for-each
   (lambda (li)
     (hashtable-update! db (lexical-info-name li)
       (lambda (prev) (cons li prev))
       '()))
   liv))

(define (slurp filename)
  (define ip (open-binary-file-to-read filename))
  (on-exit (close-port ip)
    (let go ()
      (match (fasl-read ip)
        [lexical (smash-lexical! (fasl-read ip)) (go)]
        [#!eof (void)]
        [,other (printf "IGNORING ~s\n" other) (fasl-read ip) (go)]))))
