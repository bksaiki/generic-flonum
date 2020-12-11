#lang racket

(require math/bigfloat "mpfr.rkt")

(provide gfl-exponent gfl-bits
  (contract-out
   [gfl ((or/c real? string?) . -> . gfl?)]
   [gfl? (any/c . -> . boolean?)]

   [real->gfl (real? . -> . gfl?)]
   [gfl->real (gfl? . -> . real?)]
   [bigfloat->gfl (bigfloat? . -> . gfl?)]
   [gfl->bigfloat (gfl? . -> . bigfloat?)]
   [ordinal->gfl (exact-integer? . -> . gfl?)]
   [gfl->ordinal (gfl? . -> . exact-integer?)]
   [gfl->string (gfl? . -> . string?)]

   [gflnan? (gfl? . -> . boolean?)]
   [gflinfinite? (gfl? . -> . boolean?)]
   [gflzero? (gfl? . -> . boolean?)]
   [gflnegative? (gfl? . -> . boolean?)]
   [gflpositive? (gfl? . -> . boolean?)]
   [gflsubnormal? (gfl? . -> . boolean?)]

   [gfl= (gfl? gfl? ... . -> . boolean?)]
   [gfl< (gfl? gfl? ... . -> . boolean?)]
   [gfl> (gfl? gfl? ... . -> . boolean?)]
   [gfl<= (gfl? gfl? ... . -> . boolean?)]
   [gfl>= (gfl? gfl? ... . -> . boolean?)]

   [gfl+ (gfl? ... . -> . gfl?)]
   [gfl- (gfl? gfl? ... . -> . gfl?)]
   [gfl* (gfl? ... . -> . gfl?)]
   [gfl/ (gfl? gfl? ... . -> . gfl?)]
   
   [gflsgn (gfl? . -> . boolean?)]
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

   [gflatan2 (gfl? gfl? . -> . gfl?)]
   [gflceiling (gfl? gfl? . -> . gfl?)]
   [gflcopysign (gfl? gfl? . -> . gfl?)]
   [gfldim (gfl? gfl? . -> . gfl?)]
   [gflerf (gfl? gfl? . -> . gfl?)]
   [gflerfc (gfl? gfl? . -> . gfl?)]
   [gflexpt (gfl? gfl? . -> . gfl?)]
   [gflfloor (gfl? gfl? . -> . gfl?)]
   [gflfmod (gfl? gfl? . -> . gfl?)]
   [gflgamma (gfl? gfl? . -> . gfl?)]
   [gflhypot (gfl? gfl? . -> . gfl?)]
   [gfllgamma (gfl? gfl? . -> . gfl?)]
   [gflmax (gfl? gfl? . -> . gfl?)]
   [gflmin (gfl? gfl? . -> . gfl?)]
   [gflremainder (gfl? gfl? . -> . gfl?)]
   [gflrint (gfl? gfl? . -> . gfl?)]
   [gflround (gfl? gfl? . -> . gfl?)]
   [gfltruncate (gfl? gfl? . -> . gfl?)]

   [gflfma (gfl? gfl? gfl? . -> . gfl?)]))

;;;;;;;;;;;; Parameters / Structs ;;;;;;;;;;;;   

(define gfl-exponent (make-parameter 11))
(define gfl-bits (make-parameter 64))

(struct gfl (val ex nb)
        #:name gflonum
        #:constructor-name gflonum
        #:transparent
        #:methods gen:custom-write
        [(define (write-proc x port mode)
          (fprintf port "#<gfl[~a, ~a]: ~a>" (gfl-ex x) (gfl-nb x) (gfl->string x)))])

;;;;;;;;;;;;;;;; Utility ;;;;;;;;;;;;;;;;        

(define (ex->ebounds ex sig)
  (let ([p2 (expt 2 (- ex 1))])
    (values (- 4 (+ p2 sig)) p2)))

;;;;;;;;;;;;;;;; Conversions ;;;;;;;;;;;;;;;;

(define (real->gfl x)
  (define sig (- (gfl-bits) (gfl-exponent)))
  (define-values (emin emax) (ex->ebounds (gfl-exponent) sig))
  (gflonum ((mpfr-eval emin emax sig) mpfr-set x) (gfl-exponent) (gfl-bits)))

(define (gfl->real x)
  (bigfloat->real (gfl-val x)))

(define (bigfloat->gfl x)
  (define sig (- (gfl-bits) (gfl-exponent)))
  (define-values (emin emax) (ex->ebounds (gfl-exponent) sig))
  (gflonum ((mpfr-eval emin emax sig) mpfr-set x) (gfl-exponent) (gfl-bits)))

(define (gfl->bigfloat x)
  (bfcopy (gfl-val x)))

(define (ordinal->gfl x)
  (define sig (- (gfl-bits) (gfl-exponent)))
  (define-values (emin emax) (ex->ebounds (gfl-exponent) sig))
  (gflonum ((mpfr-eval emin emax sig) (curryr ordinal->mpfr (gfl-exponent) sig) x)
       (gfl-exponent) (gfl-bits)))

(define (gfl->ordinal x)
  (define sig (- (gfl-bits) (gfl-exponent)))
  (define-values (emin emax) (ex->ebounds (gfl-ex x) sig))
  ((mpfr-eval emin emax sig) (curryr mpfr->ordinal (gfl-exponent) sig) (gfl-val x)))

(define (string->gfl x)
  (define sig (- (gfl-bits) (gfl-exponent)))
  (define-values (emin emax) (ex->ebounds (gfl-exponent) sig))
  (gflonum ((mpfr-eval emin emax sig) string->bigfloat x) (gfl-exponent) (gfl-bits)))

(define (gfl->string x)
  (define v (gfl-val x))
  (cond
   [(bfnan? v) "+nan.0"]
   [(bf= v +inf.bf) "+inf.0"]
   [(bf= v -inf.bf) "-inf.0"]
   [else (bigfloat->string v)]))

(define (gfl x)
  (cond
   [(real? x) (real->gfl x)]
   [(string? x) (string->gfl x)]))

;;;;;;;;;;;;;;;; Predicates ;;;;;;;;;;;;;;;;

(define-syntax-rule (gfl-predicate name fun)
  (define (name x) (fun (gfl-val x))))

(define-syntax-rule (gfl-predicates [name fun] ...)
  (begin (gfl-predicate name fun) ...))

(gfl-predicates
 [gflnan? bfnan?]
 [gflinfinite? bfinfinite?]
 [gflzero? bfzero?]
 [gflnegative? bfnegative?]
 [gflpositive? bfpositive?])

;;;;;;;;;;;;;;;; Comparators ;;;;;;;;;;;;;;;;

(define-syntax-rule (gfl-comparator name fun)
  (define (name head . rest)
    (let loop ([head head] [args rest])
      (cond
       [(null? args) #t]
       [(fun (gfl-val head) (gfl-val (car args)))
        (loop (car args) (cdr args))]
       [else #f]))))

(define-syntax-rule (gfl-comparators [name fun] ...)
  (begin (gfl-comparator name fun) ...))

(gfl-comparators
 [gfl=  bf=]
 [gfl<  bf<]
 [gfl>  bf>]
 [gfl<= bf<=]
 [gfl>= bf>=])

;;;;;;;;;;;;;;;; Unary operators ;;;;;;;;;;;;;;;;

(define-syntax-rule (gfl-1ary-fun name mpfr-fun)
  (define (name x)
    (define sig (- (gfl-bits) (gfl-exponent)))
    (define-values (emin emax) (ex->ebounds (gfl-exponent) sig))
    (gflonum ((mpfr-eval emin emax sig) mpfr-fun (gfl-val x))
         (gfl-exponent) (gfl-bits))))

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
    (define sig (- (gfl-bits) (gfl-exponent)))
    (define-values (emin emax) (ex->ebounds (gfl-exponent) sig))
    (gflonum ((mpfr-eval emin emax sig) mpfr-fun (gfl-val x) (gfl-val y))
         (gfl-exponent) (gfl-bits))))

(define-syntax-rule (gfl-2ary-funs [name mpfr-fun] ...)
  (begin (gfl-2ary-fun name mpfr-fun) ...))

(gfl-2ary-funs
 [gflatan2 mpfr-atan2]
 [gflceiling mpfr-ceil]
 [gflcopysign mpfr-copysign]
 [gfldim mpfr-dim]
 [gflerf mpfr-erf]
 [gflerfc mpfr-erfc]
 [gflexpt mpfr-pow]
 [gflfloor mpfr-floor]
 [gflfmod mpfr-fmod]
 [gflgamma mpfr-gamma]
 [gflhypot mpfr-hypot]
 [gfllgamma mpfr-lgamma]
 [gflmax mpfr-max]
 [gflmin mpfr-min]
 [gflremainder mpfr-remainder]
 [gflrint mpfr-rint]
 [gflround mpfr-round]
 [gfltruncate mpfr-trunc])

;;;;;;;;;;;;;;;; Ternary operators ;;;;;;;;;;;;;;;;

(define (gflfma x y z)
  (define sig (- (gfl-bits) (gfl-exponent)))
  (define-values (emin emax) (ex->ebounds (gfl-exponent) sig))
  (gflonum
    ((mpfr-eval emin emax sig) mpfr-fma (gfl-val x) (gfl-val y) (gfl-val z))
    (gfl-exponent) (gfl-bits)))

;;;;;;;;;;;;;;;;;;; Variadic operators ;;;;;;;;;;;;;;;;

(define (gfl+ . args)
  (define sig (- (gfl-bits) (gfl-exponent)))
  (define-values (emin emax) (ex->ebounds (gfl-exponent) sig))
  (gflonum 
    (let loop ([args (reverse args)])
      (cond
        [(null? args) 0.bf]
        [(= (length args) 1) (gfl-val (car args))]
        [else ((mpfr-eval emin emax sig) mpfr-add (gfl-val (car args)) (loop (cdr args)))]))
    (gfl-exponent) (gfl-bits)))

(define (gfl- head . rest)
  (define sig (- (gfl-bits) (gfl-exponent)))
  (define-values (emin emax) (ex->ebounds (gfl-exponent) sig))
  (gflonum
    (if (null? rest)
        ((mpfr-eval emin emax sig) mpfr-neg (gfl-val head))
        (let loop ([head (gfl-val head)] [args rest])
          (cond
           [(null? args) head]
           [else (loop ((mpfr-eval emin emax sig) mpfr-sub head (gfl-val (car args)))
                       (cdr args))])))
    (gfl-exponent) (gfl-bits)))

(define (gfl* . args)
  (define sig (- (gfl-bits) (gfl-exponent)))
  (define-values (emin emax) (ex->ebounds (gfl-exponent) sig))
  (gflonum 
    (let loop ([args (reverse args)])
      (cond
        [(null? args) 1.bf]
        [(= (length args) 1) (gfl-val (car args))]
        [else ((mpfr-eval emin emax sig) mpfr-mul (gfl-val (car args)) (loop (cdr args)))]))
    (gfl-exponent) (gfl-bits)))

(define (gfl/ head . rest)
  (define sig (- (gfl-bits) (gfl-exponent)))
  (define-values (emin emax) (ex->ebounds (gfl-exponent) sig))
  (gflonum 
    (let loop ([head (gfl-val head)] [args rest])
      (cond
        [(null? args) head]
        [else (loop ((mpfr-eval emin emax sig) mpfr-div head (gfl-val (car args)))
                    (cdr args))]))
    (gfl-exponent) (gfl-bits)))

;;;;;;;;;;;;;;;;;;; Miscellaneous operators ;;;;;;;;;;;;;;;;

(define (gflsgn x)
  (cond [(gflnegative? x) -1]
        [(gflpositive? x) 1]
        [else 0]))

(define (gflsubnormal? x)
  (cond
   [(or (gflnan? x) (gflinfinite? x) (gflzero? x)) #f]
   [else
    (define sig (- (gfl-bits) (gfl-exponent)))
    (define-values (emin emax) (ex->ebounds (gfl-exponent) sig))
    (define exp (+ (bigfloat-exponent (gfl-val x)) sig -1))
    (negative? (+ exp emax -2))]))