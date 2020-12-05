#lang racket

(require rackunit math/bigfloat)
(require "../generic-flonum.rkt")

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

  (define gfls (map gfl floats))
  (define gfl-2args (collect-pairs gfls))

  (for ([fl gfls])  ; real<->gfl conversion
    (check-equal? (gfl (gfl->real fl)) fl))
  (for ([fl gfls])  ; bigfloat<->gfl conversion
    (check-equal? (bigfloat->gfl (gfl->bigfloat fl)) fl))
  (for ([fl gfls])  ; ordinal<->gfl conversion
    (check-equal? (ordinal->gfl (gfl->ordinal fl)) fl))
  (for ([fl gfls])  ; ordinal<->gfl conversion
    (check-equal? (gfl (gfl->string fl)) fl))

  (define 2ary-ops
    (list (cons + gfl+) (cons - gfl-) (cons * gfl*) (cons / gfl/)
          (cons expt gflexpt) (cons atan gflatan2)))

  (for ([op 2ary-ops]) 
    (for ([gfl-arg gfl-2args] [fl-arg float-2args])
      (let ([real ((car op) (car fl-arg) (cdr fl-arg))]
            [fl ((cdr op) (car gfl-arg) (cdr gfl-arg))])
        (cond
         [(not (real? real))
          (with-check-info (['expected real] ['actual gfl])
            (check-true (gflnan? fl)))]
         [else (check-equal? fl (gfl real))]))))
)