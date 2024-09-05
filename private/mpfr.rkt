#lang racket

(require math/bigfloat)
(require "ffi.rkt")

(provide (all-defined-out)
         mpfr-rounding-mode
         mpfr-subnormalize?
         mpfr-fma
         mpfr-root
         mpfr-lgamma
         mpfr-jn
         mpfr-yn
         mpfr-sum
         mpfr-set
         mpfr-set-ebounds!)

(mpfr-0ary-funs
 [mpfr-const-pi 'mpfr_const_pi]
 [mpfr-const-log2 'mpfr_const_log2]
 [mpfr-const-euler 'mpfr_const_euler]
 [mpfr-const-catalan 'mpfr_const_catalan])

(mpfr-1ary-funs
 [mpfr-sqr 'mpfr_sqr]
 [mpfr-sqrt 'mpfr_sqrt]
 [mpfr-rec-sqrt 'mpfr_rec_sqrt]
 [mpfr-cbrt 'mpfr_cbrt]
 [mpfr-neg 'mpfr_neg]
 [mpfr-abs 'mpfr_abs]
 [mpfr-log 'mpfr_log]
 [mpfr-log2 'mpfr_log2]
 [mpfr-log10 'mpfr_log10]
 [mpfr-log1p 'mpfr_log1p]
 [mpfr-exp 'mpfr_exp]
 [mpfr-exp2 'mpfr_exp2]
 [mpfr-exp10 'mpfr_exp10]
 [mpfr-expm1 'mpfr_expm1]
 [mpfr-cos 'mpfr_cos]
 [mpfr-sin 'mpfr_sin]
 [mpfr-tan 'mpfr_tan]
 [mpfr-sec 'mpfr_sec]
 [mpfr-csc 'mpfr_csc]
 [mpfr-cot 'mpfr_cot]
 [mpfr-acos 'mpfr_acos]
 [mpfr-asin 'mpfr_asin]
 [mpfr-atan 'mpfr_atan]
 [mpfr-cosh 'mpfr_cosh]
 [mpfr-sinh 'mpfr_sinh]
 [mpfr-tanh 'mpfr_tanh]
 [mpfr-sech 'mpfr_sech]
 [mpfr-csch 'mpfr_csch]
 [mpfr-coth 'mpfr_coth]
 [mpfr-acosh 'mpfr_acosh]
 [mpfr-asinh 'mpfr_asinh]
 [mpfr-atanh 'mpfr_atanh]
 [mpfr-erf 'mpfr_erf]
 [mpfr-erfc 'mpfr_erfc]
 [mpfr-gamma 'mpfr_gamma]
 [mpfr-digamma 'mpfr_digamma]
 [mpfr-eint 'mpfr_eint]
 [mpfr-li2 'mpfr_li2]
 [mpfr-beta 'mpfr_beta]
 [mpfr-zeta 'mpfr_zeta]
 [mpfr-j0 'mpfr_j0]
 [mpfr-j1 'mpfr_j1]
 [mpfr-y0 'mpfr_y0]
 [mpfr-y1 'mpfr_y1]
 [mpfr-ai 'mpfr_ai]
 [mpfr-ceil 'mpfr_ceil]
 [mpfr-floor 'mpfr_floor] 
 [mpfr-rint 'mpfr_rint]
 [mpfr-round 'mpfr_round]
 [mpfr-trunc 'mpfr_trunc])

(mpfr-2ary-funs
 [mpfr-add 'mpfr_add]
 [mpfr-sub 'mpfr_sub]
 [mpfr-mul 'mpfr_mul]
 [mpfr-div 'mpfr_div]
 [mpfr-agm 'mpfr_agm]
 [mpfr-atan2 'mpfr_atan2]
 [mpfr-copysign 'mpfr_copysign]
 [mpfr-dim 'mpfr_dim]
 [mpfr-mod 'mpfr_fmod]
 [mpfr-hypot 'mpfr_hypot]
 [mpfr-pow 'mpfr_pow]
 [mpfr-remainder 'mpfr_remainder])

(mpfr-1ary-2val-funs
 [mpfr-sin-cos 'mpfr_sin_cos]
 [mpfr-sinh-cosh 'mpfr_sinh-cosh])
  
(define ((mpfr-eval emin emax sig) proc . args)
  (parameterize ([bf-precision sig])
    (define-values (emin* emax*) (mpfr-set-ebounds! emin emax))
    (define r (apply proc args))
    (mpfr-set-ebounds! emin* emax*)
    r))

(define (invalid-ordinal es sig)
  (expt 2 (sub1 (+ es sig))))

(define (infinite-ordinal es sig)
  (arithmetic-shift (sub1 (expt 2 es)) (sub1 sig)))

(define (ordinal->mpfr x es sig)
  (define bound (invalid-ordinal es sig))
  (unless (< (- bound) x bound)
    (error 'ordinal->mpfr "ordinal out of bounds ~a" x))
  (let loop ([x x])
    (cond
      [(zero? x) 0.bf]
      [(negative? x) (bf- (loop (- x)))]
      [(> x (infinite-ordinal es sig)) +nan.bf]
      [(= x (infinite-ordinal es sig)) +inf.bf]
      [else ; non-zero, real number
       (define msize (sub1 sig))
       (define expmin (sub1 (mpfr-get-emin)))
       (define ebits (arithmetic-shift x (- msize)))
       (define mbits (bitwise-and x (sub1 (expt 2 msize))))
       (cond
         [(zero? ebits) ; subnormal
          (bf mbits expmin)]
         [else ; normal number
          (define c (+ (expt 2 msize) mbits))
          (define exp (sub1 (+ ebits expmin)))
          (bf c exp)])])))

(define (mpfr->ordinal x es sig)
  (let loop ([x x])
    (cond
      [(bfzero? x) 0]
      [(bfnegative? x) (- (loop (bf- x)))]
      [(bfnan? x) (add1 (infinite-ordinal es sig))]
      [(bfinfinite? x) (infinite-ordinal es sig)]
      [else
       (define-values (c exp) (bigfloat->sig+exp x))
       (define expmin (sub1 (mpfr-get-emin)))
       (cond
         [(< exp expmin) ; subnormal
          (define shift (- expmin exp))
          (arithmetic-shift c (- shift))]
         [else ; normal
          (define ebits (add1 (- exp expmin)))
          (define mbits (bitwise-and c (sub1 (expt 2 (sub1 sig)))))
          (+ (arithmetic-shift ebits (sub1 sig)) mbits)])])))
