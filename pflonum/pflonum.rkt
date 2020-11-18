#lang racket

(require math/bigfloat "private/mpfr.rkt")
(provide pfl-exponent pfl-nbits
  (contract-out
   [real->pfl (real? . -> . pfl?)]
   [pfl->real (pfl? . -> . real?)]
   [bigfloat->pfl (bigfloat? . -> . pfl?)]
   [pfl->bigfloat (pfl? . -> . bigfloat?)]
   [ordinal->pfl (exact-integer? . -> . pfl?)]
   [pfl->ordinal (pfl? . -> . exact-integer?)]
   [string->pfl (string? . -> . pfl?)]
   [pfl->string (pfl? . -> . string?)]

   [pflnan? (pfl? . -> . boolean?)]
   [pflinfinite? (pfl? . -> . boolean?)]
   [pflzero? (pfl? . -> . boolean?)]
   [pflnegative? (pfl? . -> . boolean?)]
   [pflpositive? (pfl? . -> . boolean?)]

   [pflneg (pfl? . -> . pfl?)]
   [pflsqrt (pfl? . -> . pfl?)]
   [pflcbrt (pfl? . -> . pfl?)]
   [pflabs (pfl? . -> . pfl?)]
   [pfllog (pfl? . -> . pfl?)]
   [pfllog2 (pfl? . -> . pfl?)]
   [pfllog10 (pfl? . -> . pfl?)]
   [pfllog1p (pfl? . -> . pfl?)]
   [pflexp (pfl? . -> . pfl?)]
   [pflexp2 (pfl? . -> . pfl?)]
   [pflexp10 (pfl? . -> . pfl?)]
   [pflexpm1 (pfl? . -> . pfl?)]
   [pflcos (pfl? . -> . pfl?)]
   [pflsin (pfl? . -> . pfl?)]
   [pfltan (pfl? . -> . pfl?)]
   [pflsec (pfl? . -> . pfl?)]
   [pflcsc (pfl? . -> . pfl?)]
   [pflcot (pfl? . -> . pfl?)]
   [pflacos (pfl? . -> . pfl?)]
   [pflasin (pfl? . -> . pfl?)]
   [pflatan (pfl? . -> . pfl?)]
   [pflcosh (pfl? . -> . pfl?)]
   [pflsinh (pfl? . -> . pfl?)]
   [pfltanh (pfl? . -> . pfl?)]
   [pflsech (pfl? . -> . pfl?)]
   [pflcsch (pfl? . -> . pfl?)]
   [pflcoth (pfl? . -> . pfl?)]
   [pflacosh (pfl? . -> . pfl?)]
   [pflasinh (pfl? . -> . pfl?)]
   [pflatanh (pfl? . -> . pfl?)]

   [pfl+ (pfl? pfl? . -> . pfl?)]
   [pfl- (pfl? pfl? . -> . pfl?)]
   [pfl* (pfl? pfl? . -> . pfl?)]
   [pfl/ (pfl? pfl? . -> . pfl?)]
   [pflexpt (pfl? pfl? . -> . pfl?)]
   [pflatan2 (pfl? pfl? . -> . pfl?)]
   [pflremainder (pfl? pfl? . -> . pfl?)]))

;;;;;;;;;;;; Parameters / Structs ;;;;;;;;;;;;   

(define pfl-exponent (make-parameter 11))
(define pfl-nbits (make-parameter 64))
(define pfl-verbose (make-parameter #t))

(struct pfl (val ex bits)
        #:transparent
        #:methods gen:custom-write
        [(define (write-proc x port mode)
          (fprintf port "#<pfl[~a, ~a]: ~a>" (pfl-ex x) (pfl-bits x)
                        (bigfloat->string (pfl-val x))))])

;;;;;;;;;;;;;;;; Utility ;;;;;;;;;;;;;;;;        

(define (ex->ebounds ex sig)
  (let ([p2 (expt 2 (- ex 1))])
    (values (- 4 (+ p2 sig)) p2)))

;;;;;;;;;;;;;;;; Conversions ;;;;;;;;;;;;;;;;

(define (real->pfl x)
  (define sig (- (pfl-nbits) (pfl-exponent)))
  (define-values (emin emax) (ex->ebounds (pfl-exponent) sig))
  (pfl ((mpfr-eval emin emax sig) mpfr-set x) (pfl-exponent) (pfl-nbits)))

(define (pfl->real x)
  (bigfloat->real (pfl-val x)))

(define (bigfloat->pfl x)
  (define sig (- (pfl-nbits) (pfl-exponent)))
  (define-values (emin emax) (ex->ebounds (pfl-exponent) sig))
  (pfl ((mpfr-eval emin emax sig) mpfr-set x) (pfl-exponent) (pfl-nbits)))

(define (pfl->bigfloat x)
  (bfcopy (pfl-val x)))

(define (ordinal->pfl x)
  (define sig (- (pfl-nbits) (pfl-exponent)))
  (define-values (emin emax) (ex->ebounds (pfl-exponent) sig))
  (pfl ((mpfr-eval emin emax sig) (curryr ordinal->mpfr (pfl-exponent) sig) x)
       (pfl-exponent) (pfl-nbits)))

(define (pfl->ordinal x)
  (define sig (- (pfl-nbits) (pfl-exponent)))
  (define-values (emin emax) (ex->ebounds (pfl-ex x) sig))
  ((mpfr-eval emin emax sig) (curryr mpfr->ordinal (pfl-exponent) sig) (pfl-val x)))

(define (string->pfl x)
  (define sig (- (pfl-nbits) (pfl-exponent)))
  (define-values (emin emax) (ex->ebounds (pfl-exponent) sig))
  (pfl ((mpfr-eval emin emax sig) string->bigfloat x) (pfl-exponent) (pfl-nbits)))

(define (pfl->string x)
  (bigfloat->string (pfl-val x)))

;;;;;;;;;;;;;;;; Predicates ;;;;;;;;;;;;;;;;

(define-syntax-rule (pfl-predicate name fun)
  (define (name x) fun (pfl-val x)))

(define-syntax-rule (pfl-predicates [name fun] ...)
  (begin (pfl-predicate name fun) ...))

(pfl-predicates
 [pflnan? bfnan?]
 [pflinfinite? bfinfinite?]
 [pflzero? bfzero?]
 [pflnegative? bfnegative?]
 [pflpositive? bfpositive?])

;;;;;;;;;;;;;;;; Unary operators ;;;;;;;;;;;;;;;;

(define-syntax-rule (pfl-1ary-fun name mpfr-fun)
  (define (name x)
    (define sig (- (pfl-nbits) (pfl-exponent)))
    (define-values (emin emax) (ex->ebounds (pfl-exponent) sig))
    (pfl ((mpfr-eval emin emax sig) mpfr-fun (pfl-val x))
         (pfl-exponent) (pfl-nbits))))

(define-syntax-rule (pfl-1ary-funs [name mpfr-fun] ...)
  (begin (pfl-1ary-fun name mpfr-fun) ...))

(pfl-1ary-funs
 [pflsqrt mpfr-sqrt]
 [pflcbrt mpfr-cbrt]
 [pflneg mpfr-neg]
 [pflabs mpfr-abs]
 [pfllog mpfr-log]
 [pfllog2 mpfr-log2]
 [pfllog10 mpfr-log10]
 [pfllog1p mpfr-log1p]
 [pflexp mpfr-exp]
 [pflexp2 mpfr-exp2]
 [pflexp10 mpfr-exp10]
 [pflexpm1 mpfr-expm1]
 [pflcos mpfr-cos]
 [pflsin mpfr-sin]
 [pfltan mpfr-tan]
 [pflsec mpfr-sec]
 [pflcsc mpfr-csc]
 [pflcot mpfr-cot]
 [pflacos mpfr-acos]
 [pflasin mpfr-asin]
 [pflatan mpfr-atan]
 [pflcosh mpfr-cosh]
 [pflsinh mpfr-sinh]
 [pfltanh mpfr-tanh]
 [pflsech mpfr-sech]
 [pflcsch mpfr-csch]
 [pflcoth mpfr-coth]
 [pflacosh mpfr-acosh]
 [pflasinh mpfr-asinh]
 [pflatanh mpfr-atanh])

;;;;;;;;;;;;;;;; Binary operators ;;;;;;;;;;;;;;;;

(define-syntax-rule (pfl-2ary-fun name mpfr-fun)
  (define (name x y)
    (define sig (- (pfl-nbits) (pfl-exponent)))
    (define-values (emin emax) (ex->ebounds (pfl-exponent) sig))
    (pfl ((mpfr-eval emin emax sig) mpfr-fun (pfl-val x) (pfl-val y))
         (pfl-exponent) (pfl-nbits))))

(define-syntax-rule (pfl-2ary-funs [name mpfr-fun] ...)
  (begin (pfl-2ary-fun name mpfr-fun) ...))

(pfl-2ary-funs
 [pfl+ mpfr-add]
 [pfl- mpfr-sub]
 [pfl* mpfr-mul]
 [pfl/ mpfr-div]
 [pflexpt mpfr-pow]
 [pflatan2 mpfr-atan2]
 [pflremainder mpfr-remainder])