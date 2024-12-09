#lang racket/base

;; FFI bindings for MPFR

(require ffi/unsafe
         math/private/bigfloat/mpfr)

(provide (all-defined-out))

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
      (mpfr-check-range r 0 (mpfr-rounding-mode))
      (when (mpfr-subnormalize?)
        (mpfr-subnormalize r t (mpfr-rounding-mode)))
      r)))

(define-syntax-rule (mpfr-0ary-funs [name mpfr-name] ...)
  (begin
    (mpfr-0ary-fun name mpfr-name) ...))

(define-syntax-rule (mpfr-1ary-fun name mpfr-name)
  (begin
    (define fun (get-mpfr-fun mpfr-name (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))
    (define (name x)
      (define r (bf 0))
      (define t (fun r x (mpfr-rounding-mode)))
      (mpfr-check-range r 0 (mpfr-rounding-mode))
      (when (mpfr-subnormalize?)
        (mpfr-subnormalize r t (mpfr-rounding-mode)))
      r)))

(define-syntax-rule (mpfr-1ary-funs [name mpfr-name] ...)
  (begin
    (mpfr-1ary-fun name mpfr-name) ...))

(define-syntax-rule (mpfr-2ary-fun name mpfr-name)
  (begin
    (define fun
      (get-mpfr-fun mpfr-name (_fun _mpfr-pointer _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))
    (define (name x y)
      (define r (bf 0))
      (define t (fun r x y (mpfr-rounding-mode)))
      (mpfr-check-range r 0 (mpfr-rounding-mode))
      (when (mpfr-subnormalize?)
        (mpfr-subnormalize r t (mpfr-rounding-mode)))
      r)))

(define-syntax-rule (mpfr-2ary-funs [name mpfr-name] ...)
  (begin
    (mpfr-2ary-fun name mpfr-name) ...))

(define-syntax-rule (mpfr-1ary-2val-fun name mpfr-name)
  (begin
    (define fun
      (get-mpfr-fun 'mpfr_sin_cos (_fun _mpfr-pointer _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))
    (define (name x)
      (define r0 (bf 0))
      (define r1 (bf 1))
      (define t (fun r0 r1 x (mpfr-rounding-mode)))
      (define-values (c s) (quotient/remainder t 4))
      (mpfr-check-range r0 0 (mpfr-rounding-mode))
      (mpfr-check-range r1 0 (mpfr-rounding-mode))
      (when (mpfr-subnormalize?)
        (mpfr-subnormalize r0 s (mpfr-rounding-mode))
        (mpfr-subnormalize r1 t (mpfr-rounding-mode)))
      (values r0 r1))))

(define-syntax-rule (mpfr-1ary-2val-funs [name mpfr-name] ...)
  (begin
    (mpfr-1ary-2val-fun name mpfr-name) ...))

(define mpfr-fma-fun
  (get-mpfr-fun 'mpfr_fma
                (_fun _mpfr-pointer _mpfr-pointer _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))

(define (mpfr-fma x y z)
  (define r (bf 0))
  (define t (mpfr-fma-fun r x y z (mpfr-rounding-mode)))
  (mpfr-check-range r 0 (mpfr-rounding-mode))
  (when (mpfr-subnormalize?)
    (mpfr-subnormalize r t (mpfr-rounding-mode)))
  r)

(define mpfr-root-fun
  (get-mpfr-fun 'mpfr_root (_fun _mpfr-pointer _mpfr-pointer _uint _rnd_t -> _int)))

(define (mpfr-root x n)
  (define r (bf 0))
  (define t (mpfr-root-fun r x n (mpfr-rounding-mode)))
  (mpfr-check-range r 0 (mpfr-rounding-mode))
  (when (mpfr-subnormalize?)
    (mpfr-subnormalize r t (mpfr-rounding-mode)))
  r)

(define mpfr-lgamma-fun
  (get-mpfr-fun 'mpfr_lgamma (_fun _mpfr-pointer _pointer _mpfr-pointer _rnd_t -> _int)))

(define (mpfr-lgamma x)
  (define r (bf 0))
  (define s (malloc _int))
  (define t (mpfr-lgamma-fun r s x (mpfr-rounding-mode)))
  (mpfr-check-range r 0 (mpfr-rounding-mode))
  (when (mpfr-subnormalize?)
    (mpfr-subnormalize r t (mpfr-rounding-mode)))
  r)

(define mpfr-jn-fun (get-mpfr-fun 'mpfr_jn (_fun _mpfr-pointer _long _mpfr-pointer _rnd_t -> _int)))

(define (mpfr-jn n x)
  (define r (bf 0))
  (define t (mpfr-jn-fun r n x (mpfr-rounding-mode)))
  (mpfr-check-range r 0 (mpfr-rounding-mode))
  (when (mpfr-subnormalize?)
    (mpfr-subnormalize r t (mpfr-rounding-mode)))
  r)

(define mpfr-yn-fun (get-mpfr-fun 'mpfr_yn (_fun _mpfr-pointer _long _mpfr-pointer _rnd_t -> _int)))

(define (mpfr-yn n x)
  (define r (bf 0))
  (define t (mpfr-yn-fun r n x (mpfr-rounding-mode)))
  (mpfr-check-range r 0 (mpfr-rounding-mode))
  (when (mpfr-subnormalize?)
    (mpfr-subnormalize r t (mpfr-rounding-mode)))
  r)

(define (mpfr-set x)
  (define v (if (bigfloat? x) (bfcopy x) (bf x)))
  (mpfr-check-range v 0 (mpfr-rounding-mode))
  (when (mpfr-subnormalize?)
    (mpfr-subnormalize v 0 (mpfr-rounding-mode)))
  v)

(define (mpfr-set-ebounds! emin emax)
  (let ([emin* (mpfr-get-emin)] [emax* (mpfr-get-emax)])
    (mpfr-set-emin! emin)
    (mpfr-set-emax! emax)
    (values emin* emax*)))
