#lang racket

(require rackunit math/bigfloat)
(require "../pflonum.rkt")

(define (random-double)
  (define parts (make-list 4 (random (expt 2 16))))
  (define bytes
    (bitwise-ior
      (first parts)
      (arithmetic-shift (second parts) 16)
      (arithmetic-shift (third parts) 32)
      (arithmetic-shift (fourth parts) 48)))
  (floating-point-bytes->real (integer->integer-bytes bytes 8 #f)))

(define (collect-pairs li)
  (let loop ([li li])
    (if (< (length li) 2)
        '()
        (cons (cons (car li) (cadr li)) (loop (cddr li))))))

(module+ test
  (define floats (make-list 1000 (random-double)))
  (define float-2args (collect-pairs floats))

  (define pfls (map real->pfl floats))
  (define pfl-2args (collect-pairs pfls))

  (for ([fl pfls])  ; real<->pfl conversion
    (check-equal? (real->pfl (pfl->real fl)) fl))
  (for ([fl pfls])  ; bigfloat<->pfl conversion
    (check-equal? (bigfloat->pfl (pfl->bigfloat fl)) fl))
  (for ([fl pfls])  ; ordinal<->pfl conversion
    (check-equal? (ordinal->pfl (pfl->ordinal fl)) fl))
  (for ([fl pfls])  ; ordinal<->pfl conversion
    (check-equal? (string->pfl (pfl->string fl)) fl))

  (define 2ary-ops
    (list (cons + pfl+) (cons - pfl-) (cons * pfl*) (cons / pfl/)
          (cons expt pflexpt) (cons atan pflatan2)))

  (for ([op 2ary-ops]) 
    (for ([pfl-arg pfl-2args] [fl-arg float-2args])
      (let ([real ((car op) (car fl-arg) (cdr fl-arg))]
            [pfl ((cdr op) (car pfl-arg) (cdr pfl-arg))])
        (cond
         [(not (real? real))
          (with-check-info (['expected real] ['actual pfl])
            (check-true (pflnan? pfl)))]
         [else (check-equal? pfl (real->pfl real))]))))
)