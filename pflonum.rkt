#lang racket

(require math/bigfloat "private/mpfr.rkt")
(provide
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

(struct pfl (val ex bits)
        #:transparent
        #:methods gen:custom-write
        [(define (write-proc x port mode)
          (fprintf port "#<pfl[~a, ~a]: ~a>" (pfl-ex x) (pfl-bits x) (pfl-val x)))])

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

(define (pflnan? x)
  (bfnan? (pfl-val x)))

(define (pflinfinite? x)
  (bfinfinite? (pfl-val x)))

;;;;;;;;;;;;;;;; Unary operators ;;;;;;;;;;;;;;;;

(define-syntax-rule (float-1ary-fun name mpfr-fun)
  (define (name x)
    (define sig (- (pfl-nbits) (pfl-exponent)))
    (define-values (emin emax) (ex->ebounds (pfl-exponent) sig))
    (pfl ((mpfr-eval emin emax sig) mpfr-fun (pfl-val x))
         (pfl-exponent) (pfl-nbits))))

(define-syntax-rule (float-1ary-funs [name mpfr-fun] ...)
  (begin (float-1ary-fun name mpfr-fun) ...))


;;;;;;;;;;;;;;;; Binary operators ;;;;;;;;;;;;;;;;

(define-syntax-rule (float-2ary-fun name mpfr-fun)
  (define (name x y)
    (define sig (- (pfl-nbits) (pfl-exponent)))
    (define-values (emin emax) (ex->ebounds (pfl-exponent) sig))
    (pfl ((mpfr-eval emin emax sig) mpfr-fun (pfl-val x) (pfl-val y))
         (pfl-exponent) (pfl-nbits))))

(define-syntax-rule (float-2ary-funs [name mpfr-fun] ...)
  (begin (float-2ary-fun name mpfr-fun) ...))

(float-2ary-funs
 [pfl+ mpfr-add]
 [pfl- mpfr-sub]
 [pfl* mpfr-mul]
 [pfl/ mpfr-div]
 [pflexpt mpfr-pow]
 [pflatan2 mpfr-atan2]
 [pflremainder mpfr-remainder])