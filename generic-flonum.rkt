#lang racket

(require math/bigfloat "private/mpfr.rkt")
(provide gfl-exponent gfl-nbits
  (contract-out
   [real->gfl (real? . -> . gfl?)]
   [gfl->real (gfl? . -> . real?)]
   [bigfloat->gfl (bigfloat? . -> . gfl?)]
   [gfl->bigfloat (gfl? . -> . bigfloat?)]
   [ordinal->gfl (exact-integer? . -> . gfl?)]
   [gfl->ordinal (gfl? . -> . exact-integer?)]
   [string->gfl (string? . -> . gfl?)]
   [gfl->string (gfl? . -> . string?)]

   [gflnan? (gfl? . -> . boolean?)]
   [gflinfinite? (gfl? . -> . boolean?)]
   [gflzero? (gfl? . -> . boolean?)]
   [gflnegative? (gfl? . -> . boolean?)]
   [gflpositive? (gfl? . -> . boolean?)]

   [gflneg (gfl? . -> . gfl?)]
   [gflsqrt (gfl? . -> . gfl?)]
   [gflcbrt (gfl? . -> . gfl?)]
   [gflabs (gfl? . -> . gfl?)]
   [gfllog (gfl? . -> . gfl?)]
   [gfllog2 (gfl? . -> . gfl?)]
   [gfllog10 (gfl? . -> . gfl?)]
   [gfllog1p (gfl? . -> . gfl?)]
   [gflexp (gfl? . -> . gfl?)]
   [gflexp2 (gfl? . -> . gfl?)]
   [gflexp10 (gfl? . -> . gfl?)]
   [gflexpm1 (gfl? . -> . gfl?)]
   [gflcos (gfl? . -> . gfl?)]
   [gflsin (gfl? . -> . gfl?)]
   [gfltan (gfl? . -> . gfl?)]
   [gflsec (gfl? . -> . gfl?)]
   [gflcsc (gfl? . -> . gfl?)]
   [gflcot (gfl? . -> . gfl?)]
   [gflacos (gfl? . -> . gfl?)]
   [gflasin (gfl? . -> . gfl?)]
   [gflatan (gfl? . -> . gfl?)]
   [gflcosh (gfl? . -> . gfl?)]
   [gflsinh (gfl? . -> . gfl?)]
   [gfltanh (gfl? . -> . gfl?)]
   [gflsech (gfl? . -> . gfl?)]
   [gflcsch (gfl? . -> . gfl?)]
   [gflcoth (gfl? . -> . gfl?)]
   [gflacosh (gfl? . -> . gfl?)]
   [gflasinh (gfl? . -> . gfl?)]
   [gflatanh (gfl? . -> . gfl?)]

   [gfl+ (gfl? gfl? . -> . gfl?)]
   [gfl- (gfl? gfl? . -> . gfl?)]
   [gfl* (gfl? gfl? . -> . gfl?)]
   [gfl/ (gfl? gfl? . -> . gfl?)]
   [gflexpt (gfl? gfl? . -> . gfl?)]
   [gflatan2 (gfl? gfl? . -> . gfl?)]
   [gflremainder (gfl? gfl? . -> . gfl?)]))

;;;;;;;;;;;; Parameters / Structs ;;;;;;;;;;;;   

(define gfl-exponent (make-parameter 11))
(define gfl-nbits (make-parameter 64))
(define gfl-verbose (make-parameter #t))

(struct gfl (val ex bits)
        #:transparent
        #:methods gen:custom-write
        [(define (write-proc x port mode)
          (fprintf port "#<gfl[~a, ~a]: ~a>" (gfl-ex x) (gfl-bits x)
                        (bigfloat->string (gfl-val x))))])

;;;;;;;;;;;;;;;; Utility ;;;;;;;;;;;;;;;;        

(define (ex->ebounds ex sig)
  (let ([p2 (expt 2 (- ex 1))])
    (values (- 4 (+ p2 sig)) p2)))

;;;;;;;;;;;;;;;; Conversions ;;;;;;;;;;;;;;;;

(define (real->gfl x)
  (define sig (- (gfl-nbits) (gfl-exponent)))
  (define-values (emin emax) (ex->ebounds (gfl-exponent) sig))
  (gfl ((mpfr-eval emin emax sig) mpfr-set x) (gfl-exponent) (gfl-nbits)))

(define (gfl->real x)
  (bigfloat->real (gfl-val x)))

(define (bigfloat->gfl x)
  (define sig (- (gfl-nbits) (gfl-exponent)))
  (define-values (emin emax) (ex->ebounds (gfl-exponent) sig))
  (gfl ((mpfr-eval emin emax sig) mpfr-set x) (gfl-exponent) (gfl-nbits)))

(define (gfl->bigfloat x)
  (bfcopy (gfl-val x)))

(define (ordinal->gfl x)
  (define sig (- (gfl-nbits) (gfl-exponent)))
  (define-values (emin emax) (ex->ebounds (gfl-exponent) sig))
  (gfl ((mpfr-eval emin emax sig) (curryr ordinal->mpfr (gfl-exponent) sig) x)
       (gfl-exponent) (gfl-nbits)))

(define (gfl->ordinal x)
  (define sig (- (gfl-nbits) (gfl-exponent)))
  (define-values (emin emax) (ex->ebounds (gfl-ex x) sig))
  ((mpfr-eval emin emax sig) (curryr mpfr->ordinal (gfl-exponent) sig) (gfl-val x)))

(define (string->gfl x)
  (define sig (- (gfl-nbits) (gfl-exponent)))
  (define-values (emin emax) (ex->ebounds (gfl-exponent) sig))
  (gfl ((mpfr-eval emin emax sig) string->bigfloat x) (gfl-exponent) (gfl-nbits)))

(define (gfl->string x)
  (bigfloat->string (gfl-val x)))

;;;;;;;;;;;;;;;; Predicates ;;;;;;;;;;;;;;;;

(define-syntax-rule (gfl-predicate name fun)
  (define (name x) fun (gfl-val x)))

(define-syntax-rule (gfl-predicates [name fun] ...)
  (begin (gfl-predicate name fun) ...))

(gfl-predicates
 [gflnan? bfnan?]
 [gflinfinite? bfinfinite?]
 [gflzero? bfzero?]
 [gflnegative? bfnegative?]
 [gflpositive? bfpositive?])

;;;;;;;;;;;;;;;; Unary operators ;;;;;;;;;;;;;;;;

(define-syntax-rule (gfl-1ary-fun name mpfr-fun)
  (define (name x)
    (define sig (- (gfl-nbits) (gfl-exponent)))
    (define-values (emin emax) (ex->ebounds (gfl-exponent) sig))
    (gfl ((mpfr-eval emin emax sig) mpfr-fun (gfl-val x))
         (gfl-exponent) (gfl-nbits))))

(define-syntax-rule (gfl-1ary-funs [name mpfr-fun] ...)
  (begin (gfl-1ary-fun name mpfr-fun) ...))

(gfl-1ary-funs
 [gflsqrt mpfr-sqrt]
 [gflcbrt mpfr-cbrt]
 [gflneg mpfr-neg]
 [gflabs mpfr-abs]
 [gfllog mpfr-log]
 [gfllog2 mpfr-log2]
 [gfllog10 mpfr-log10]
 [gfllog1p mpfr-log1p]
 [gflexp mpfr-exp]
 [gflexp2 mpfr-exp2]
 [gflexp10 mpfr-exp10]
 [gflexpm1 mpfr-expm1]
 [gflcos mpfr-cos]
 [gflsin mpfr-sin]
 [gfltan mpfr-tan]
 [gflsec mpfr-sec]
 [gflcsc mpfr-csc]
 [gflcot mpfr-cot]
 [gflacos mpfr-acos]
 [gflasin mpfr-asin]
 [gflatan mpfr-atan]
 [gflcosh mpfr-cosh]
 [gflsinh mpfr-sinh]
 [gfltanh mpfr-tanh]
 [gflsech mpfr-sech]
 [gflcsch mpfr-csch]
 [gflcoth mpfr-coth]
 [gflacosh mpfr-acosh]
 [gflasinh mpfr-asinh]
 [gflatanh mpfr-atanh])

;;;;;;;;;;;;;;;; Binary operators ;;;;;;;;;;;;;;;;

(define-syntax-rule (gfl-2ary-fun name mpfr-fun)
  (define (name x y)
    (define sig (- (gfl-nbits) (gfl-exponent)))
    (define-values (emin emax) (ex->ebounds (gfl-exponent) sig))
    (gfl ((mpfr-eval emin emax sig) mpfr-fun (gfl-val x) (gfl-val y))
         (gfl-exponent) (gfl-nbits))))

(define-syntax-rule (gfl-2ary-funs [name mpfr-fun] ...)
  (begin (gfl-2ary-fun name mpfr-fun) ...))

(gfl-2ary-funs
 [gfl+ mpfr-add]
 [gfl- mpfr-sub]
 [gfl* mpfr-mul]
 [gfl/ mpfr-div]
 [gflexpt mpfr-pow]
 [gflatan2 mpfr-atan2]
 [gflremainder mpfr-remainder])