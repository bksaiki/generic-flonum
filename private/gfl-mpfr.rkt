#lang racket

(require math/bigfloat "mpfr.rkt")

(provide gfl-exponent
         gfl-bits
         gfl?
         real->gfl
         gfl->real
         bigfloat->gfl 
         gfl->bigfloat
         ordinal->gfl
         gfl->ordinal
         string->gfl
         gfl->string
         gflfma
         gfl+
         gfl-
         gfl*
         gfl/
         gflsgn
         gflsubnormal?)

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

;;;;;;;;;;;;;;;; Predicates ;;;;;;;;;;;;;;;;

(define-syntax-rule (gfl-predicate name fun)
  (begin
    (define (name x) (fun (gfl-val x)))
    (provide name)))

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
  (begin
    (define (name head . rest)
      (let loop ([head head] [args rest])
        (cond
        [(null? args) #t]
        [(fun (gfl-val head) (gfl-val (car args)))
          (loop (car args) (cdr args))]
        [else #f])))
    (provide name)))

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
  (begin
    (define (name x)
      (define sig (- (gfl-bits) (gfl-exponent)))
      (define-values (emin emax) (ex->ebounds (gfl-exponent) sig))
      (gflonum ((mpfr-eval emin emax sig) mpfr-fun (gfl-val x))
          (gfl-exponent) (gfl-bits)))
    (provide name)))

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
  (begin
    (define (name x y)
      (define sig (- (gfl-bits) (gfl-exponent)))
      (define-values (emin emax) (ex->ebounds (gfl-exponent) sig))
      (gflonum ((mpfr-eval emin emax sig) mpfr-fun (gfl-val x) (gfl-val y))
          (gfl-exponent) (gfl-bits)))
    (provide name)))

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

;;;;;;;;;;;;;;;;;;; Constants ;;;;;;;;;;;;;;;;

(define-for-syntax const-funs (list))
(provide (for-syntax const-funs))

(define-syntax-rule (gfl-0ary-fun name mpfr-fun)
  (begin
    (define (name)
      (define sig (- (gfl-bits) (gfl-exponent)))
      (define-values (emin emax) (ex->ebounds (gfl-exponent) sig))
      (gflonum ((mpfr-eval emin emax sig) mpfr-fun) (gfl-exponent) (gfl-bits)))
    (provide name)
    (begin-for-syntax (set! const-funs (cons #'name const-funs)))))

(define-syntax-rule (gfl-0ary-funs [name mpfr-fun] ...)
  (begin (gfl-0ary-fun name mpfr-fun) ...))

(gfl-0ary-funs
 [pi.gfl mpfr-const-pi]
 [ln2.gfl mpfr-const-log2])

(define-syntax-rule (gfl-0ary-const name val)
  (begin
    (define (name) val)
    (provide name)
    (begin-for-syntax (set! const-funs (cons #'name const-funs)))))

(define-syntax-rule (gfl-0ary-consts [name val] ...)
  (begin (gfl-0ary-const name val) ...))

(gfl-0ary-consts
 [e.gfl (gflexp (real->gfl 1))]
 [log2e.gfl (gfllog2 (gflexp (real->gfl 1)))]
 [log10e.gfl (gfllog10 (gflexp (real->gfl 1)))]
 [ln10.gfl (gfllog (real->gfl 10))]
 [pi/2.gfl (gflasin (real->gfl 1))]
 [pi/4.gfl (gflatan2 (real->gfl +inf.0) (real->gfl +inf.0))]
 [1/pi.gfl (gfl/ (real->gfl 1) (gflatan2 (real->gfl 0) (real->gfl -1)))]
 [2/pi.gfl (gfl/ (real->gfl 2) (gflatan2 (real->gfl 0) (real->gfl -1)))]
 [2/sqrtpi.gfl (gfl/ (real->gfl 2) (gflsqrt (gflatan2 (real->gfl 0) (real->gfl -1))))]
 [sqrt2.gfl (gflsqrt (real->gfl 2))]
 [sqrt1/2.pi.gfl (gflsqrt (real->gfl 1/2))])