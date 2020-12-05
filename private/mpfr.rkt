#lang racket

(require math/bigfloat)
(provide (all-defined-out) (all-from-out math/bigfloat)
         mpfr-set mpfr-fma)

;;; Isolate FFI code
(module hairy racket/base
  (require ffi/unsafe math/private/bigfloat/mpfr)
  (provide mpfr-set mpfr-set-ebounds! mpfr-get-emin mpfr-get-emax
           mpfr-fma mpfr-1ary-funs mpfr-2ary-funs)

  ;;; Additional MPFR functions
  (define mpfr-get-emin (get-mpfr-fun 'mpfr_get_emin (_fun -> _exp_t)))
  (define mpfr-get-emax (get-mpfr-fun 'mpfr_get_emax (_fun -> _exp_t)))
  (define mpfr-set-emin! (get-mpfr-fun 'mpfr_set_emin (_fun _exp_t -> _int)))
  (define mpfr-set-emax! (get-mpfr-fun 'mpfr_set_emax (_fun _exp_t -> _int)))

  (define mpfr-subnormalize (get-mpfr-fun 'mpfr_subnormalize (_fun _mpfr-pointer _int _rnd_t -> _int)))
  (define mpfr-check-range (get-mpfr-fun 'mpfr_check_range (_fun _mpfr-pointer _int _rnd_t -> _int)))

  (define-syntax-rule (mpfr-1ary-fun name mpfr-name)
    (begin
      (define fun (get-mpfr-fun mpfr-name (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))
      (define (name x)
        (define r (bf 0))
        (define t (fun r x (bf-rounding-mode)))
        (mpfr-subnormalize r t (bf-rounding-mode))
        r)))

  (define-syntax-rule (mpfr-1ary-funs [name mpfr-name] ...)
    (begin (mpfr-1ary-fun name mpfr-name) ...))

  (define-syntax-rule (mpfr-2ary-fun name mpfr-name)
    (begin
      (define fun (get-mpfr-fun mpfr-name (_fun _mpfr-pointer _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))
      (define (name x y)
        (define r (bf 0))
        (define t (fun r x y (bf-rounding-mode)))
        (mpfr-subnormalize r t (bf-rounding-mode))
        r)))

  (define-syntax-rule (mpfr-2ary-funs [name mpfr-name] ...)
    (begin (mpfr-2ary-fun name mpfr-name) ...))

  (define (mpfr-fma x y z)
    (define fun (get-mpfr-fun 'mpfr_fma (_fun _mpfr-pointer _mpfr-pointer _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))
    (define r (bf 0))
    (define t (fun r x y z (bf-rounding-mode)))
    (mpfr-subnormalize r t (bf-rounding-mode))
    r)

  (define (mpfr-set x)
    (define v (if (bigfloat? x) (bfcopy x) (bf x)))
    (mpfr-check-range v 0 (bf-rounding-mode))
    (mpfr-subnormalize v 0 (bf-rounding-mode))
    v)

  (define (mpfr-set-ebounds! emin emax)
    (let ([emin* (mpfr-get-emin)]
          [emax* (mpfr-get-emax)])
      (mpfr-set-emin! emin)
      (mpfr-set-emax! emax)
      (values emin* emax*)))
)

(require (submod "." hairy))
;;; End FFI code

(mpfr-1ary-funs
 [mpfr-sqrt 'mpfr_sqrt]
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
 [mpfr-atanh 'mpfr_atanh])

(mpfr-2ary-funs
 [mpfr-add 'mpfr_add]
 [mpfr-sub 'mpfr_sub]
 [mpfr-mul 'mpfr_mul]
 [mpfr-div 'mpfr_div]
 [mpfr-pow 'mpfr_pow]
 [mpfr-atan2 'mpfr_atan2]
 [mpfr-remainder 'mpfr_remainder])
  
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
      (define diff (- x (expt 2 exp-offset)))
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