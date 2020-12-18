#lang scribble/manual
@require[@for-label[generic-flonum
                    racket/base
                    racket/contract
                    math
                    math/bigfloat]]

@(define bigfloat-link (hyperlink "http://docs.racket-lang.org/math/bigfloat.html" "math/bigfloat"))

@title{generic-flonum}
@author{Brett Saiki}

@defmodule[generic-flonum]

This library provides an alternate interface to @hyperlink["http://mpfr.org" "MPFR"] (compared to @bigfloat-link), emphasizing the emulation of unusual floating-point types such as @italic{binary128}, @italic{binary16}, @italic{bfloat16}, etc.

@section{Motivations}

While the @bigfloat-link interface is sufficient for most high-precision computing, it is lacking in a couple areas. Mainly, it does not
@itemlist[#:style 'ordered
          @item{properly emulate subnormal values}
          @item{allow the exponent range to be changed}]
If the user's intent is to compute, say @racket[(exp -10)] and @racket[(exp 20)], to find a close estimate of their real values, then neither of the problems above matter. However, if the user wanted to know their properly rounded values in half-precision, then both problems matter.

At half-precision, @racket[(exp -10)] and @racket[(exp 20)] evaluate to @racket[4.5419e-5] and @racket[+inf.0], respectively. On the other hand, evaluating @racket[(bfexp (bf -10))] and @racket[(bfexp (bf -10))] with @racket[(bf-precision 11)] returns @racket[(bf "4.5389e-5")] and @racket[(bf "#e4.8523e8")], respectively. While the latter results are certainly more accurate, they do not reflect the proper behavior for overflow and subnormal values.

@section{Type and Constructors}

@defproc[(gfl? [v any/c])
         boolean?]{
  Returns @racket[#t] if @racket[v] is a generic-flonum.
}

@defproc[(gfl [x (or/c string? real?)])
         gfl?]{
  Constructs a generic-flonum from a string or a real number.
}

@defproc[(gflcopy [x gfl?])
          gfl?]{
  Returns a generic-flonum with the same value as @racket[x] except rounded at the current
  precision and rounding mode.
}

@section{Conversions}

@deftogether[(@defproc[(real->gfl [v real?]) gfl?]
              @defproc[(gfl->real [x gfl?]) real?])]{
  Converts a real @racket[v] to a generic-flonum @racket[x] and back.
}

@deftogether[(@defproc[(bigfloat->gfl [v bigfloat?]) gfl?]
              @defproc[(gfl->bigfloat [x gfl?]) bigfloat?])]{
  Converts a bigfloat @racket[v] to a generic-flonum @racket[x] and back.
}

@deftogether[(@defproc[(ordinal->gfl [o exact-integer?]) gfl?]
              @defproc[(gfl->ordinal [x gfl?]) exact-integer?])]{
  Like @racket[ordinal->flonum] and @racket[flonum->ordinal] but for generic-flonums.
}

@deftogether[(@defproc[(string->gfl [s string?]) gfl?]
              @defproc[(gfl->string [x gfl?]) string?])]{
  Converts a string @racket[s] to a generic-flonum @racket[x] and back.
}

@section{Parameters}

The standard bigfloat library defines @racket[(bf-precision)] and @racket[(bf-rounding-mode)] to control
significand size and the rounding of results. The following parameters are similar and can be used at the
same time as their bigfloat counterparts, although this behavior may change in the future.

@defparam[gfl-exponent es exact-positive-integer?
          #:value 11]{
  A parameter that defines the current exponent size of values returned from most functions
  in this library. Default value is the exponent size of a flonum. This parameter has a guard
  that ensures @racket[(gfl-exponent)] is less than @racket[(gfl-bits)].
}

@defparam[gfl-bits nb exact-positive-integer?
          #:value 64]{
  A parameter that defines the current sum of the exponent size and significand size of
  values returned from most functions in this library. More concisely, it defines the significand
  size indirectly. The significand size corresponds to @racket[(bf-precision)]) and is equal to
  @racket[(- (gfl-bits) (gfl-exponent))]. Default value is the length of a flonum in bits.
  This parameter has a guard that ensures @racket[(gfl-bits)] is more than @racket[(gfl-exponent)].
}

@defparam[gfl-rounding-mode rm (symbols 'nearest 'zero 'up 'down 'away)
          #:value 'nearest]{
  A parameter that determines the mode used to round the result of most functions in this library.
  Note that @bigfloat-link accepts all values but the last for @racket[(bf-rounding-mode)].
}

@section{Predicates}

@deftogether[(@defproc[(gflzero? [x gfl?]) boolean?]
              @defproc[(gflpositive? [x gfl?]) boolean?]
              @defproc[(gflnegative [x gfl?]) boolean?]
              @defproc[(gflinfinite? [x gfl?]) boolean?]
              @defproc[(gflnan? [x gfl?]) boolean?])]{
  Unary predicates corresponding to @racket[zero?], @racket[positive?], @racket[negative?],
  @racket[infinite?], and @racket[nan?].
}

@defproc[(gflsubnormal? [x gfl?])
          boolean?]{
  Returns @racket[#t] if @racket[x] is a subnormal value.
}

@deftogether[(@defproc[(gfl= [x gfl?]) boolean?]
              @defproc[(gfl> [x gfl?]) boolean?]
              @defproc[(gfl< [x gfl?]) boolean?]
              @defproc[(gfl>= [x gfl?]) boolean?]
              @defproc[(gfl<= [x gfl?]) boolean?])]{
  Standard comparators corresponding to @racket[=], @racket[>], @racket[<],
  @racket[>=], and @racket[<=]. Infinities are larger or smaller than any
  other value, and comparing to @racket[+nan.gfl] always returns @racket[#f].
}

@section{Mathematical Operations}

@deftogether[(@defproc[(gfl+ [x gfl?] ...) gfl?]
              @defproc[(gfl- [x gfl?] [y gfl?] ...) gfl?]
              @defproc[(gfl* [x gfl?] ...) gfl?]
              @defproc[(gfl/ [x gfl?] [y gfl?] ...) gfl?]
              @defproc[(gflsqr [x gfl?]) gfl?]
              @defproc[(gflabs [x gfl?]) gfl?]
              @defproc[(gflsgn [x gfl?]) gfl?])]{
  Standard arithmetic functions, corresponding to @racket[+], @racket[-], @racket[*], @racket[/],
  @racket[sqr], @racket[abs], @racket[sgn]. Similar to @racket[bf/], division by zero returns
  @racket[+nan.gfl].
}

@deftogether[(@defproc[(gflsqrt [x gfl?]) gfl?]
              @defproc[(gfl1/sqrt [x gfl?]) gfl?]
              @defproc[(gflcbrt [x gfl?]) gfl?])]{
  Returns the square root, the reciprocal square root, and the cube root of @racket[x].
}

@defproc[(gflroot [x gfl?] [n exact-nonnegative-integer?])
          gfl?]{
  Returns the @racket[n]-th root of @racket[x]. @racket[n] must be a nonnegative fixnum.
}

@defproc[(gflhypot [x gfl?])
          gfl?]{
  Returns @racket[(gflsqrt (gfl+ (gflsqr x) (gflsqr y)))] without intermediate overflow or rounding.
}

@deftogether[(@defproc[(gfllog [x gfl?]) gfl?]
              @defproc[(gfllog2 [x gfl?]) gfl?]
              @defproc[(gfllog10 [x gfl?]) gfl?])]{
  Returns the log of @racket[x] in base @italic{e}, 2, and 10.
}

@deftogether[(@defproc[(gflexp [x gfl?]) gfl?]
              @defproc[(gflexp2 [x gfl?]) gfl?]
              @defproc[(gflexp10 [x gfl?]) gfl?])]{
  Returns the exponential of @racket[x] in base @italic{e}, 2, and 10.
}

@deftogether[(@defproc[(gfllog1p [x gfl?]) gfl?]
              @defproc[(gflexpm1 [x gfl?]) gfl?])]{
  Computes @racket[(gfllog (gfl+ 1.gfl x))] and @racket[(gfl- (gflexp x) 1.gfl)].
}

@defproc[(gflexpt [x gfl?])
          gfl?]{
  Function corresponding to @racket[expt].
}

@deftogether[(@defproc[(gflsin [x gfl?]) gfl?]
              @defproc[(gflcos [x gfl?]) gfl?]
              @defproc[(gfltan [x gfl?]) gfl?]
              @defproc[(gflasin [x gfl?]) gfl?]
              @defproc[(gflacos [x gfl?]) gfl?]
              @defproc[(gflatan [x gfl?]) gfl?]
              @defproc[(gflatan2 [x gfl?] [y gfl?]) gfl?])]{
  Standard trigonmetric functions and their inverses.
}

@deftogether[(@defproc[(gflsinh [x gfl?]) gfl?]
              @defproc[(gflcosh [x gfl?]) gfl?]
              @defproc[(gfltanh [x gfl?]) gfl?]
              @defproc[(gflasinh [x gfl?]) gfl?]
              @defproc[(gflacosh [x gfl?]) gfl?]
              @defproc[(gflatanh [x gfl?]) gfl?])]{
  Standard hyperbolic functions and their inverses.
}

@deftogether[(@defproc[(gflcsc [x gfl?]) gfl?]
              @defproc[(gflsec [x gfl?]) gfl?]
              @defproc[(gflcot [x gfl?]) gfl?])]{
  Standard reciprocal trigonometric functions.
}

@deftogether[(@defproc[(gflcsch [x gfl?]) gfl?]
              @defproc[(gflsech [x gfl?]) gfl?]
              @defproc[(gflcoth [x gfl?]) gfl?])]{
  Standard reciprocal hyperbolic functions.
}