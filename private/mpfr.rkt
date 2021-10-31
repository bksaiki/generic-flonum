#lang racket

(require math/bigfloat)

(provide (all-defined-out)
         (all-from-out (submod "." hairy)))

;;; Isolate FFI code
(module hairy racket/base
  (require ffi/unsafe math/private/bigfloat/mpfr)
  (provide mpfr-set mpfr-set-ebounds! mpfr-get-emin mpfr-get-emax
           mpfr-0ary-funs mpfr-1ary-funs mpfr-2ary-funs mpfr-1ary-2val-funs
           mpfr-rounding-mode mpfr-subnormalize?
           mpfr-sum mpfr-fma mpfr-root
           mpfr-jn mpfr-yn
           mpfr-lgamma)

  ;; Override _rnd_t type from bigfloat
  (define _rnd_t (_enum '(nearest zero up down away)))

  ;; Separate rounding mode parameter
  (define mpfr-rounding-mode (make-parameter 'nearest))

  ;; Subnormalize parameter
  (define mpfr-subnormalize? (make-parameter #t))

  ;;; Additional MPFR functions
  (define mpfr-get-emin (get-mpfr-fun 'mpfr_get_emin (_fun -> _exp_t)))
  (define mpfr-get-emax (get-mpfr-fun 'mpfr_get_emax (_fun -> _exp_t)))
  (define mpfr-set-emin! (get-mpfr-fun 'mpfr_set_emin (_fun _exp_t -> _int)))
  (define mpfr-set-emax! (get-mpfr-fun 'mpfr_set_emax (_fun _exp_t -> _int)))

  (define mpfr-subnormalize (get-mpfr-fun 'mpfr_subnormalize (_fun _mpfr-pointer _int _rnd_t -> _int)))
  (define mpfr-check-range (get-mpfr-fun 'mpfr_check_range (_fun _mpfr-pointer _int _rnd_t -> _int)))

  (define-syntax-rule (mpfr-0ary-fun name mpfr-name)
    (begin
      (define fun (get-mpfr-fun mpfr-name (_fun _mpfr-pointer _rnd_t -> _int)))
      (define (name)
        (define r (bf 0))
        (define t (fun r (mpfr-rounding-mode)))
        (when (mpfr-subnormalize?) (mpfr-subnormalize r t (mpfr-rounding-mode)))
        (mpfr-check-range r 0 (mpfr-rounding-mode))
        r)))

  (define-syntax-rule (mpfr-0ary-funs [name mpfr-name] ...)
    (begin (mpfr-0ary-fun name mpfr-name) ...))

  (define-syntax-rule (mpfr-1ary-fun name mpfr-name)
    (begin
      (define fun (get-mpfr-fun mpfr-name (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))
      (define (name x)
        (define r (bf 0))
        (define t (fun r x (mpfr-rounding-mode)))
        (when (mpfr-subnormalize?) (mpfr-subnormalize r t (mpfr-rounding-mode)))
        (mpfr-check-range r 0 (mpfr-rounding-mode))
        r)))

  (define-syntax-rule (mpfr-1ary-funs [name mpfr-name] ...)
    (begin (mpfr-1ary-fun name mpfr-name) ...))

  (define-syntax-rule (mpfr-2ary-fun name mpfr-name)
    (begin
      (define fun (get-mpfr-fun mpfr-name (_fun _mpfr-pointer _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))
      (define (name x y)
        (define r (bf 0))
        (define t (fun r x y (mpfr-rounding-mode)))
        (when (mpfr-subnormalize?) (mpfr-subnormalize r t (mpfr-rounding-mode)))
        (mpfr-check-range r 0 (mpfr-rounding-mode))
        r)))

  (define-syntax-rule (mpfr-2ary-funs [name mpfr-name] ...)
    (begin (mpfr-2ary-fun name mpfr-name) ...))

  (define-syntax-rule (mpfr-1ary-2val-fun name mpfr-name)
    (begin
      (define fun (get-mpfr-fun 'mpfr_sin_cos (_fun _mpfr-pointer _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))
      (define (name x)
        (define r0 (bf 0))
        (define r1 (bf 1))
        (define t (fun r0 r1 x (mpfr-rounding-mode)))
        (define-values (c s) (quotient/remainder t 4))
        (when (mpfr-subnormalize?)
          (mpfr-subnormalize r0 s (mpfr-rounding-mode))
          (mpfr-subnormalize r1 t (mpfr-rounding-mode)))
        (mpfr-check-range r0 0 (mpfr-rounding-mode))
        (mpfr-check-range r1 0 (mpfr-rounding-mode))
        (values r0 r1))))

  (define-syntax-rule (mpfr-1ary-2val-funs [name mpfr-name] ...)
    (begin (mpfr-1ary-2val-fun name mpfr-name) ...))

  (define mpfr-fma-fun
    (get-mpfr-fun 'mpfr_fma (_fun _mpfr-pointer _mpfr-pointer _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

  (define (mpfr-fma x y z)
    (define r (bf 0))
    (define t (mpfr-fma-fun r x y z (mpfr-rounding-mode)))
    (when (mpfr-subnormalize?) (mpfr-subnormalize r t (mpfr-rounding-mode)))
    (mpfr-check-range r 0 (mpfr-rounding-mode))
    r)

  (define mpfr-root-fun
    (get-mpfr-fun 'mpfr_root (_fun _mpfr-pointer _mpfr-pointer _uint _rnd_t -> _int)))

  (define (mpfr-root x n)
    (define r (bf 0))
    (define t (mpfr-root-fun r x n (mpfr-rounding-mode)))
    (when (mpfr-subnormalize?) (mpfr-subnormalize r t (mpfr-rounding-mode)))
    (mpfr-check-range r 0 (mpfr-rounding-mode))
    r)

  (define mpfr-lgamma-fun
    (get-mpfr-fun 'mpfr_lgamma (_fun _mpfr-pointer _pointer _mpfr-pointer _rnd_t -> _int)))

  (define (mpfr-lgamma x)
    (define r (bf 0))
    (define s (malloc _int))
    (define t (mpfr-lgamma-fun r s x (mpfr-rounding-mode)))
    (when (mpfr-subnormalize?) (mpfr-subnormalize r t (mpfr-rounding-mode)))
    (mpfr-check-range r 0 (mpfr-rounding-mode))
    r)

  (define mpfr-jn-fun
    (get-mpfr-fun 'mpfr_jn (_fun _mpfr-pointer _long _mpfr-pointer _rnd_t -> _int)))

  (define (mpfr-jn n x)
    (define r (bf 0))
    (define t (mpfr-jn-fun r n x (mpfr-rounding-mode)))
    (when (mpfr-subnormalize?) (mpfr-subnormalize r t (mpfr-rounding-mode)))
    (mpfr-check-range r 0 (mpfr-rounding-mode))
    r)

  (define mpfr-yn-fun
    (get-mpfr-fun 'mpfr_yn (_fun _mpfr-pointer _long _mpfr-pointer _rnd_t -> _int)))

  (define (mpfr-yn n x)
    (define r (bf 0))
    (define t (mpfr-yn-fun r n x (mpfr-rounding-mode)))
    (when (mpfr-subnormalize?) (mpfr-subnormalize r t (mpfr-rounding-mode)))
    (mpfr-check-range r 0 (mpfr-rounding-mode))
    r)

  (define mpfr-sum-fun
    (get-mpfr-fun 'mpfr_sum (_fun _mpfr-pointer (_list i _mpfr-pointer) _ulong _rnd_t -> _int)))

  (define (mpfr-sum xs)
    (define r (bf 0))
    (define t (mpfr-sum-fun r xs (length xs) (mpfr-rounding-mode)))
    (when (mpfr-subnormalize?) (mpfr-subnormalize r t (mpfr-rounding-mode)))
    r)

  (define (mpfr-set x)
    (define v (if (bigfloat? x) (bfcopy x) (bf x)))
    (when (mpfr-subnormalize?) (mpfr-subnormalize v 0 (mpfr-rounding-mode)))
    (mpfr-check-range v 0 (mpfr-rounding-mode))
    v)

  (define (mpfr-set-ebounds! emin emax)
    (let ([emin* (mpfr-get-emin)]
          [emax* (mpfr-get-emax)])
      (mpfr-set-emin! emin)
      (mpfr-set-emax! emax)
      (values emin* emax*)))
)
;;; End FFI code

(require (submod "." hairy))

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

(define (ordinal->mpfr x es sig)
  (define x* (abs x))
  (define infty (* (expt 2 (- sig 1)) (- (expt 2 es) 1)))
  (define val
    (cond
     [(> x* infty) +nan.bf]
     [(= x* infty) +inf.bf]
     [(zero? x*) 0.bf]
     [(< x* (expt 2 (- sig 1)))
      (define unit (ordinal->bigfloat 1))
      (define exp-offset
        (for/fold ([l 0]) ([i (in-naturals 1)])
              #:break (> (expt 2 i) x*)
          i))
      (define base (bfshift unit exp-offset))
      (define diff (- x* (expt 2 exp-offset)))
      (bf+ base (bf* unit (bf diff)))]
     [else
      (ordinal->bigfloat (+ x* (* (- sig 2) (expt 2 (- sig 1))) 1))]))
  (if (negative? x) (bf- val) val))

(define (mpfr->ordinal x es sig)
  (define x* (bfabs x))
  (define enorm (- 2 (mpfr-get-emax)))
  (define ex (+ (bigfloat-exponent x*) (- sig 1)))
  (define val
    (cond
     [(bfnan? x*)
      (+ (* (expt 2 (- sig 1)) (- (expt 2 es) 1)) 1)]
     [(bf= x* +inf.bf)
      (* (expt 2 (- sig 1)) (- (expt 2 es) 1))]
     [(bfzero? x*) 0]
     [(< ex enorm)
      (define unit (ordinal->bigfloat 1))
      (define exp-offset (+ 1 (- ex (mpfr-get-emin))))
      (define base (bfshift unit exp-offset))
      (define accum (expt 2 exp-offset))
      (define diff (bf- x* base))
      (+ accum (bigfloat->real (bfround (bf/ diff unit))))]
     [else
      (- (bigfloat->ordinal x*) (* (- sig 2) (expt 2 (- sig 1))) 1)]))
  (if (bfnegative? x) (- val) val))
