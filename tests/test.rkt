#lang racket

(require rackunit)
(require "../main.rkt")

(module hairy racket/base
  (require ffi/unsafe math/private/bigfloat/mpfr)
  (provide mpfr-get-version)
  (define mpfr-get-version (get-mpfr-fun 'mpfr_get_version (_fun -> _string))))

(require (submod "." hairy))

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

(define floats (make-list 1000 (random-double)))
(define float-2args (collect-pairs floats))

(define gfls (map gfl floats))
(define gfl-2args (collect-pairs gfls))

;; Conversion test

(for ([fl gfls])  ; real<->gfl conversion
(check-equal? (gfl (gfl->real fl)) fl))
(for ([fl gfls])  ; bigfloat<->gfl conversion
(check-equal? (bigfloat->gfl (gfl->bigfloat fl)) fl))
(for ([fl gfls])  ; ordinal<->gfl conversion
(check-equal? (ordinal->gfl (gfl->ordinal fl)) fl))
(for ([fl gfls])  ; string<->gfl conversion
(check-equal? (gfl (gfl->string fl)) fl))

(define 2ary-ops
(list (cons + gfl+) (cons - gfl-) (cons * gfl*) (cons / gfl/)
      (cons expt gflexpt) (cons atan gflatan2)))

; arithmetic tests

(for ([op 2ary-ops]) 
(for ([gfl-arg gfl-2args] [fl-arg float-2args])
(let ([real ((car op) (car fl-arg) (cdr fl-arg))]
      [fl ((cdr op) (car gfl-arg) (cdr gfl-arg))])
      (cond
      [(not (real? real))
      (with-check-info (['expected real] ['actual gfl])
      (check-true (gflnan? fl)))]
      [else (check-equal? fl (gfl real))]))))

;; The rest are adapted from the math-test package

;; Exact tests

(gfl-bits 128)

(check-equal? (gfl->string (gfl "123456.25"))
              "123456.25")

(check gfl= (gfl 1) (gfl 1.0))

;; Constants

(check-equal? pi.gfl (gfl #e3.141592653589793238462643383279502884195))
(check-equal? euler.gfl (gfl #e0.5772156649015328606065120900824024310432))
(check-equal? catalan.gfl (gfl #e0.9159655941772190150546035149323841107734))
(check-equal? log2.gfl (gfl #e0.6931471805599453094172321214581765680748))
(check-equal? phi.gfl (gfl #e1.61803398874989484820458683436563811772))

;; Special values

(check-eqv? (gfl->real +nan.gfl) +nan.0)
(check-eqv? (gfl->real +inf.gfl) +inf.0)
(check-eqv? (gfl->real -inf.gfl) -inf.0)
; (check-eqv? (gfl->real -0.gfl) -0.0)
(check-eqv? (gfl->real 0.gfl) 0)
(check-equal? (gfl- +inf.gfl) -inf.gfl)
(check-equal? (gfl- -inf.gfl) +inf.gfl)
(check-equal? (gfl- +nan.gfl) +nan.gfl)
; (check-equal? (gfl- 0.gfl) -0.gfl)
; (check-equal? (gfl- -0.gfl) 0.gfl)

(for ([bits '(13 15 29 27 43 75 139 14 16 18 22 24 140)])
  (parameterize ([gfl-bits bits])
    ;; +max.gfl/-max.gfl
   ; (check-equal? (gfl- +max.gfl) -max.gfl)
    (check-equal? (- (gfl->ordinal +inf.gfl) 1) (gfl->ordinal +max.gfl))
    (check-equal? (+ (gfl->ordinal -inf.gfl) 1) (gfl->ordinal -max.gfl))
    (check-equal? (gflnext +max.gfl) +inf.gfl)
    (check-equal? (gflprev +inf.gfl) +max.gfl)
    (check-equal? (gflprev -max.gfl) -inf.gfl)
    (check-equal? (gflnext -inf.gfl) -max.gfl)
    ;; +min.gfl/-min.gfl
    (check-equal? (gfl- +min.gfl) -min.gfl)
    (check-equal? (gfl* (gfl 0.5) +min.gfl)  0.gfl)
    (check-equal? (gfl* (gfl 0.5) -min.gfl) -0.gfl)
    (check-equal? (gfl->ordinal +min.gfl)  1)
    (check-equal? (gfl->ordinal -min.gfl) -1)
    (check-equal? (gflprev +min.gfl) 0.gfl)
    (check-equal? (gflnext 0.gfl) +min.gfl)
    (check-equal? (gflnext -min.gfl) -0.gfl)
    (check-equal? (gflprev 0.gfl) -min.gfl)
    ))

;; Sanity tests

(gfl-bits 64)
(gfl-exponent 11)

(for ([f+xs+ys
        (list
        (list
          gflsqr
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl 0.gfl +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list +inf.gfl +inf.gfl 1.gfl 0.gfl 0.gfl 0.gfl 0.gfl 1.gfl +inf.gfl +inf.gfl +nan.gfl))
        (list
          gflsqrt
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl 0.gfl
                +min.gfl 1.gfl +max.gfl
                +inf.gfl +nan.gfl)
          (list +nan.gfl +nan.gfl +nan.gfl +nan.gfl -0.gfl 0.gfl
                (gfl "2.2227587494850775e-162") 1.gfl (gfl "1.3407807929942596e154")
                +inf.gfl +nan.gfl))
        (list
          gflcbrt
          (list -inf.gfl
                -max.gfl -1.gfl -min.gfl
                -0.gfl 0.gfl
                +min.gfl 1.gfl +max.gfl
                +inf.gfl +nan.gfl)
          (list -inf.gfl
                (gfl "-5.6438030941223623e102") -1.gfl (gfl "-1.7031839360032603e-108")
                -0.gfl 0.gfl
                (gfl "1.7031839360032603e-108") 1.gfl (gfl "5.6438030941223623e102")
                +inf.gfl +nan.gfl))
        (list
          gfl-
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl 0.gfl +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list +inf.gfl +max.gfl 1.gfl +min.gfl 0.gfl -0.gfl -min.gfl -1.gfl -max.gfl -inf.gfl +nan.gfl))
        (list
          gfl/
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list -0.gfl (gfl "-5.5626846462680035e-309") -1.gfl -inf.gfl -inf.gfl
                +inf.gfl +inf.gfl 1.gfl (gfl "5.5626846462680035e-309") 0.gfl +nan.gfl))
        (list
          gflabs
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl 0.gfl +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list +inf.gfl +max.gfl 1.gfl +min.gfl 0.gfl 0.gfl +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl))
        (list
          gfllog
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl 0.gfl
                +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list +nan.gfl +nan.gfl +nan.gfl +nan.gfl -inf.gfl -inf.gfl
                (gfl -744.44007192138122) 0.gfl (gfl 709.78271289338397) +inf.gfl +nan.gfl))
        (list
          gfllog2
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl 0.gfl
                +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list +nan.gfl +nan.gfl +nan.gfl +nan.gfl -inf.gfl -inf.gfl
                (gfl -1074) 0.gfl (gfl 1024) +inf.gfl +nan.gfl))
        (list
          gfllog10
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl 0.gfl
                +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list +nan.gfl +nan.gfl +nan.gfl +nan.gfl -inf.gfl -inf.gfl
                (gfl -323.30621534311581) 0.gfl (gfl 308.25471555991675) +inf.gfl +nan.gfl))
        (list
          gfllog1p
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl 0.gfl +min.gfl
                1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list +nan.gfl +nan.gfl -inf.gfl -min.gfl -0.gfl 0.gfl +min.gfl
                (gfl #e0.69314718055994529) (gfl 709.78271289338397) +inf.gfl +nan.gfl))
        (list
          gflexp
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list 0.gfl 0.gfl (gfl #e0.36787944117144233) 1.gfl 1.gfl
                1.gfl 1.gfl (gfl #e2.7182818284590451) +inf.gfl +inf.gfl +nan.gfl))
        (list
          gflexp2
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl 0.gfl +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list 0.gfl 0.gfl (gfl #e0.5) 1.gfl 1.gfl 1.gfl 1.gfl 2.gfl +inf.gfl +inf.gfl +nan.gfl))
        (list
          gflexp10
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl 0.gfl +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list 0.gfl 0.gfl (gfl #e0.1) 1.gfl 1.gfl 1.gfl 1.gfl 10.gfl +inf.gfl +inf.gfl +nan.gfl))
        (list
          gflexpm1
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list -1.gfl -1.gfl (gfl #e-0.63212055882855767) -min.gfl -0.gfl
                0.gfl +min.gfl (gfl #e1.7182818284590453) +inf.gfl +inf.gfl +nan.gfl))
        (list
          gflsin
          (list -inf.gfl -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl +inf.gfl +nan.gfl)
          (list +nan.gfl (gfl #e-0.8414709848078965) -min.gfl -0.gfl
                0.gfl +min.gfl (gfl #e0.8414709848078965) +nan.gfl +nan.gfl))
        (list
          gflcos
          (list -inf.gfl -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl +inf.gfl +nan.gfl)
          (list +nan.gfl (gfl #e0.54030230586813977) 1.gfl 1.gfl
                1.gfl 1.gfl (gfl #e0.54030230586813977) +nan.gfl +nan.gfl))
        (list
          gfltan
          (list -inf.gfl -1.gfl -min.gfl -0.gfl 0.gfl +min.gfl 1.gfl +inf.gfl +nan.gfl)
          (list +nan.gfl (gfl #e-1.5574077246549023) -min.gfl -0.gfl
                0.gfl +min.gfl (gfl #e1.5574077246549023) +nan.gfl +nan.gfl))
        (list
          gflcsc
          (list -inf.gfl -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl +inf.gfl +nan.gfl)
          (list +nan.gfl (gfl #e-1.1883951057781212) -inf.gfl -inf.gfl
                +inf.gfl +inf.gfl (gfl #e1.1883951057781212) +nan.gfl +nan.gfl))
        (list
          gflsec
          (list -inf.gfl -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl +inf.gfl +nan.gfl)
          (list +nan.gfl (gfl #e1.8508157176809257) 1.gfl 1.gfl
                1.gfl 1.gfl (gfl #e1.8508157176809257) +nan.gfl +nan.gfl))
        (list
          gflcot
          (list -inf.gfl -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl +inf.gfl +nan.gfl)
          (list +nan.gfl (gfl #e-0.64209261593433076) -inf.gfl -inf.gfl
                +inf.gfl +inf.gfl (gfl #e0.64209261593433076) +nan.gfl +nan.gfl))
        (list
          gflasin
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list +nan.gfl +nan.gfl (gfl #e-1.5707963267948966) -min.gfl -0.gfl
                0.gfl +min.gfl (gfl #e1.5707963267948966) +nan.gfl +nan.gfl +nan.gfl))
        (list
          gflacos
          (list -inf.gfl -max.gfl
                -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl
                +max.gfl +inf.gfl +nan.gfl)
          (list +nan.gfl +nan.gfl
                (gfl #e3.1415926535897931) (gfl #e1.5707963267948966) (gfl #e1.5707963267948966)
                (gfl #e1.5707963267948966) (gfl #e1.5707963267948966) 0.gfl
                +nan.gfl +nan.gfl +nan.gfl))
        (list
          gflatan
          (list -inf.gfl -max.gfl
                -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl
                +max.gfl +inf.gfl +nan.gfl)
          (list (gfl #e-1.5707963267948966) (gfl #e-1.5707963267948966)
                (gfl #e-0.78539816339744828) -min.gfl -0.gfl
                0.gfl +min.gfl (gfl #e0.78539816339744828)
                (gfl #e1.5707963267948966) (gfl #e1.5707963267948966) +nan.gfl))
        (list
          gflsinh
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list -inf.gfl -inf.gfl (gfl #e-1.1752011936438014) -min.gfl -0.gfl
                0.gfl +min.gfl (gfl #e1.1752011936438014) +inf.gfl +inf.gfl +nan.gfl))
        (list
          gflcosh
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list +inf.gfl +inf.gfl (gfl #e1.5430806348152437) 1.gfl 1.gfl
                1.gfl 1.gfl (gfl #e1.5430806348152437) +inf.gfl +inf.gfl +nan.gfl))
        (list
          gfltanh
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list -1.gfl -1.gfl (gfl #e-0.76159415595576485) -min.gfl -0.gfl
                0.gfl +min.gfl (gfl #e0.76159415595576485) 1.gfl 1.gfl +nan.gfl))
        (list
          gflcsch
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list -0.gfl -0.gfl (gfl #e-0.85091812823932156) -inf.gfl -inf.gfl
                +inf.gfl +inf.gfl (gfl #e0.85091812823932156) 0.gfl 0.gfl +nan.gfl))
        (list
          gflsech
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list 0.gfl 0.gfl (gfl #e0.64805427366388535) 1.gfl 1.gfl
                1.gfl 1.gfl (gfl #e0.64805427366388535) 0.gfl 0.gfl +nan.gfl))
        (list
          gflcoth
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list -1.gfl -1.gfl (gfl #e-1.3130352854993312) -inf.gfl -inf.gfl
                +inf.gfl +inf.gfl (gfl #e1.3130352854993312) 1.gfl 1.gfl +nan.gfl))
        (list
          gflasinh
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list -inf.gfl (gfl -710.47586007394398) (gfl #e-0.88137358701954305) -min.gfl -0.gfl
                0.gfl +min.gfl (gfl #e0.88137358701954305) (gfl 710.47586007394398) +inf.gfl +nan.gfl))
        (list
          gflacosh
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl 0.gfl +min.gfl 1.gfl
                +max.gfl +inf.gfl +nan.gfl)
          (list +nan.gfl +nan.gfl +nan.gfl +nan.gfl +nan.gfl +nan.gfl +nan.gfl 0.gfl
                (gfl 710.47586007394398) +inf.gfl +nan.gfl))
        (list
          gflatanh
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl 0.gfl +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list +nan.gfl +nan.gfl -inf.gfl -min.gfl -0.gfl 0.gfl +min.gfl +inf.gfl +nan.gfl +nan.gfl +nan.gfl))
        (list
          gfleint
          (list -inf.gfl -max.gfl
                -1.gfl -min.gfl
                -0.gfl 0.gfl +min.gfl
                1.gfl +max.gfl +inf.gfl +nan.gfl)
          (append
          ;; MPFR changed handling of negative arguments between version 3.x and 4.x:
          (if (regexp-match? #rx"^3[.]" (mpfr-get-version))
              (list +nan.gfl +nan.gfl
                    +nan.gfl +nan.gfl)
              (list -0.gfl (gfl -4.9406564584124654e-324)
                    (gfl #e-0.21938393439552029) (gfl -743.86285625647974)))
          (list -inf.gfl -inf.gfl (gfl -743.86285625647974)
                (gfl #e1.8951178163559368) +inf.gfl +inf.gfl +nan.gfl)))
        (list
          gflli2
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list -inf.gfl (gfl -251897.39469521283) (gfl #e-0.8224670334241132) -min.gfl -0.gfl
                0.gfl +min.gfl (gfl #e1.6449340668482264) (gfl -251892.4598930123) -inf.gfl +nan.gfl))
        (list
          gflgamma
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl 0.gfl +min.gfl 1.gfl +max.gfl +inf.gfl
                +nan.gfl)
          (list +nan.gfl +nan.gfl +nan.gfl -inf.gfl -inf.gfl +inf.gfl +inf.gfl 1.gfl +inf.gfl +inf.gfl
                +nan.gfl))
        #;
        (list
          gfldigamma
          (list -inf.gfl -1.gfl -min.gfl -0.gfl 0.gfl +min.gfl
                1.gfl +inf.gfl +nan.gfl)
          (list +nan.gfl +nan.gfl +inf.gfl +inf.gfl -inf.gfl -inf.gfl
                (gfl #e-0.57721566490153287) +inf.gfl +nan.gfl))
        (list
          gflzeta
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list +nan.gfl 0.gfl (gfl #e-0.083333333333333329) (gfl #e-0.5) (gfl #e-0.5)
                (gfl #e-0.5) (gfl #e-0.5) +inf.gfl 1.gfl 1.gfl +nan.gfl))
        (list
          gflerf
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl +max.gfl +inf.gfl
                +nan.gfl)
          (list -1.gfl -1.gfl (gfl #e-0.84270079294971489) (gfl -4.9406564584124654e-324) -0.gfl
                0.gfl (gfl 4.9406564584124654e-324) (gfl #e0.84270079294971489) 1.gfl 1.gfl
                +nan.gfl))
        (list
          gflerfc
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl 
                0.gfl +min.gfl 1.gfl +max.gfl +inf.gfl +nan.gfl)
          (list 2.gfl 2.gfl (gfl #e1.8427007929497148) 1.gfl 1.gfl
                1.gfl 1.gfl (gfl #e0.15729920705028513) 0.gfl 0.gfl +nan.gfl))
        (list
          gflj0
          (list -inf.gfl -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl +inf.gfl +nan.gfl)
          (list 0.gfl (gfl #e0.76519768655796661) 1.gfl 1.gfl
                1.gfl 1.gfl (gfl #e0.76519768655796661) 0.gfl +nan.gfl))
        (list
          gflj1
          (list -inf.gfl -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl +inf.gfl +nan.gfl)
          (list 0.gfl (gfl #e-0.4400505857449335) -0.gfl -0.gfl
                0.gfl 0.gfl (gfl #e0.4400505857449335) 0.gfl +nan.gfl))
        (list
          (λ (x) (gfljn 0 x))
          (list -inf.gfl -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl +inf.gfl +nan.gfl)
          (list 0.gfl (gfl #e0.76519768655796661) 1.gfl 1.gfl
                1.gfl 1.gfl (gfl #e0.76519768655796661) 0.gfl +nan.gfl))
        (list
          (λ (x) (gfljn 1 x))
          (list -inf.gfl -1.gfl -min.gfl -0.gfl
                0.gfl +min.gfl 1.gfl +inf.gfl +nan.gfl)
          (list 0.gfl (gfl #e-0.4400505857449335) -0.gfl -0.gfl
                0.gfl 0.gfl (gfl #e0.4400505857449335) 0.gfl +nan.gfl))
        (list
          gfly0
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl 0.gfl
                +min.gfl 1.gfl +inf.gfl +nan.gfl)
          (list +nan.gfl +nan.gfl +nan.gfl +nan.gfl -inf.gfl -inf.gfl
                (gfl -473.99907342300429) (gfl #e0.088256964215676956) 0.gfl +nan.gfl))
        (list
          gfly1
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl 0.gfl +min.gfl
                1.gfl +inf.gfl +nan.gfl)
          (list +nan.gfl +nan.gfl +nan.gfl +nan.gfl -inf.gfl -inf.gfl -inf.gfl
                (gfl #e-0.78121282130028868) 0.gfl +nan.gfl))
        (list
          (λ (x) (gflyn 0 x))
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl 0.gfl
                +min.gfl 1.gfl +inf.gfl +nan.gfl)
          (list +nan.gfl +nan.gfl +nan.gfl +nan.gfl -inf.gfl -inf.gfl
                (gfl -473.99907342300429) (gfl #e0.088256964215676956) 0.gfl +nan.gfl))
        (list
          (λ (x) (gflyn 1 x))
          (list -inf.gfl -max.gfl -1.gfl -min.gfl -0.gfl 0.gfl +min.gfl
                1.gfl +inf.gfl +nan.gfl)
          (list +nan.gfl +nan.gfl +nan.gfl +nan.gfl -inf.gfl -inf.gfl -inf.gfl
                (gfl #e-0.78121282130028868) 0.gfl +nan.gfl)))])

  (match-define (list f xs ys) f+xs+ys)
  (for ([x  (in-list xs)]
        [y  (in-list ys)])
    (define y0 (f x))
    (unless (equal? y y0)
      (printf "f = ~a  x = ~v  y = ~v~n" f x y))
    (check-equal? y0 y)))