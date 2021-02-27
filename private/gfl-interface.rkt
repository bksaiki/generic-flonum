#lang racket

(require math/bigfloat "gfl.rkt")

(provide
  (contract-out
   [gfl-exponent (parameter/c exact-positive-integer?)]
   [gfl-bits (parameter/c exact-positive-integer?)]
   [gfl-rounding-mode (parameter/c (symbols 'nearest 'zero 'up 'down 'away))]
   [gfl-verbose? (parameter/c boolean?)]

   [gfl ((or/c real? string?) . -> . gfl?)]
   [gfl? (any/c . -> . boolean?)]

   [real->gfl (real? . -> . gfl?)]
   [gfl->real (gfl? . -> . real?)]
   [bigfloat->gfl (bigfloat? . -> . gfl?)]
   [gfl->bigfloat (gfl? . -> . bigfloat?)]
   [ordinal->gfl (exact-integer? . -> . gfl?)]
   [gfl->ordinal (gfl? . -> . exact-integer?)]
   [gfl->string (gfl? . -> . string?)]
   [gflcopy (gfl? . -> . gfl?)]

   [gflnan? (gfl? . -> . boolean?)]
   [gflinfinite? (gfl? . -> . boolean?)]
   [gflzero? (gfl? . -> . boolean?)]
   [gflnegative? (gfl? . -> . boolean?)]
   [gflpositive? (gfl? . -> . boolean?)]
   [gflinteger? (gfl? . -> . boolean?)]
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
   [gflmax (gfl? ... . -> . gfl?)]
   [gflmin (gfl? ... . -> . gfl?)]
   
   [gflsgn (gfl? . -> . gfl?)]
   [gflsqr (gfl? . -> . gfl?)]
   [gflsqrt (gfl? . -> . gfl?)]
   [gfl1/sqrt (gfl . -> . gfl?)]
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
   [gflerf (gfl? . -> . gfl?)]
   [gflerfc (gfl? . -> . gfl?)]
   [gflgamma (gfl? . -> . gfl?)]
   [gfllgamma (gfl? . -> . gfl?)]
   [gfldigamma (gfl? . -> . gfl?)]
   [gfleint (gfl? . -> . gfl?)]
   [gflli2 (gfl? . -> . gfl?)]
   [gflbeta (gfl? . -> . gfl?)]
   [gflzeta (gfl? . -> . gfl?)]
   [gflj0 (gfl? . -> . gfl?)]
   [gflj1 (gfl? . -> . gfl?)]
   [gfly0 (gfl? . -> . gfl?)]
   [gfly1 (gfl? . -> . gfl?)]
   [gflai (gfl? . -> . gfl?)]

   [gflceiling (gfl? . -> . gfl?)]
   [gflfloor (gfl? . -> . gfl?)]
   [gflrint (gfl? . -> . gfl?)]
   [gflround (gfl? . -> . gfl?)]
   [gfltruncate (gfl? . -> . gfl?)]

   [gflagm (gfl? gfl? . -> . gfl?)]
   [gflatan2 (gfl? gfl? . -> . gfl?)]
   [gflcopysign (gfl? gfl? . -> . gfl?)]
   [gfldim (gfl? gfl? . -> . gfl?)]
   [gflexpt (gfl? gfl? . -> . gfl?)]
   [gflmod (gfl? gfl? . -> . gfl?)]
   [gflhypot (gfl? gfl? . -> . gfl?)]
   [gflremainder (gfl? gfl? . -> . gfl?)]

   [gflfma (gfl? gfl? gfl? . -> . gfl?)]
   [gflsin+cos (gfl? . -> . (values gfl? gfl?))]
   [gflsinh+cosh (gfl? . -> . (values gfl? gfl?))]
   [gflroot (gfl? natural? . -> . gfl?)]
   [gfljn (gfl-long/c gfl? . -> . gfl?)]
   [gflyn (gfl-long/c gfl? . -> . gfl?)]

   [gfls-between (gfl? gfl? . -> . exact-integer?)]
   [gflnext (gfl? . -> . gfl?)]
   [gflprev (gfl? . ->  . gfl?)]
   [gflstep (gfl? exact-integer? . -> . gfl?)]))

(define (gfl x)
  (cond
   [(real? x) (real->gfl x)]
   [(string? x) (string->gfl x)]))

(define gfl-long/c
  (flat-contract-with-explanation
    (λ (v)
     (cond
      [(and exact-integer? (< -2147483648 v 2147483648)) #t]
      [else
       (λ (blame)
        (raise-blame-error blame v
          '(expected: "an exact integer between -2147483648 and 2147483648" given: "~e")
            v))]))
    #:name 'gfl-long/c))