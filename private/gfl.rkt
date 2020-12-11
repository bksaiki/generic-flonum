#lang racket

(require math/bigfloat "gfl-mpfr.rkt")

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

(define (gfl x)
  (cond
   [(real? x) (real->gfl x)]
   [(string? x) (string->gfl x)]))