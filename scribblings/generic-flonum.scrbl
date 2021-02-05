#lang scribble/manual
@require[@for-label[generic-flonum
                    racket/base
                    racket/contract
                    math
                    math/bigfloat]]

@(define bigfloat-link (hyperlink "http://docs.racket-lang.org/math/bigfloat.html" "math/bigfloat"))

@title{Generic Flonums}
@author[(author+email "Brett Saiki" "bksaiki@gmail.com")]

@defmodule[generic-flonum]

This library provides an alternate interface to @hyperlink["http://mpfr.org" "MPFR"] (compared to @bigfloat-link), emphasizing the emulation of floating-point
formats such as @italic{binary128}, @italic{binary16}, @italic{bfloat16}, etc.

@section{Introduction}

While the @bigfloat-link interface is sufficient for most high-precision computing, it is lacking in a couple areas. Mainly, it does not
@itemlist[#:style 'ordered
          @item{properly emulate subnormal arithmetic}
          @item{allow the exponent range to be changed}]
Normally, neither of these problems cause concern. For example, if a user intends to find an approximate value
for some computation on the reals, then subnormal arithmetic or a narrower exponent range is not particular useful.
However, if a user wants to know the result of a computation specifically in some format, say half-precision, then
@bigfloat-link is insufficient.

At half-precision, @racket[(exp -10)] and @racket[(exp 20)] evaluate to @racket[4.5419e-5] and @racket[+inf.0], respectively. On the other hand,
evaluating @racket[(bfexp (bf -10))] and @racket[(bfexp (bf -10))] with @racket[(bf-precision 11)] returns @racket[(bf "4.5389e-5")] and
@racket[(bf "#e4.8523e8")]. While the latter results are certainly more accurate, they do not reflect proper behavior in half-precision.
The standard bigfloat library does not subnormalize the first result (no subnormal arithmetic), nor does it recognize the overflow in the
second result (fixed exponent range).

This library fixes the issues mentioned above by automatically emulating subnormal arithmetic when
necessary and providing a way to change the exponent range. In addition, the interface is quite
similar to @bigfloat-link, so it will feel familiar to anyone who has used the standard bigfloat library
before. There are also a few extra operations from the C math library such as @racket[gflfma], @racket[gflmod], and
@racket[gflremainder] that the bigfloat library does not support.

See @bigfloat-link for more information on bigfloats.

@section{Type and Constructors}

A generic flonum is a wrapper that stores a bigfloat and the significand and exponent size
when it was initialized.

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
  in this library. Default value is the exponent size of a flonum.
}

@defparam[gfl-bits nb exact-positive-integer?
          #:value 64]{
  A parameter that defines the current sum of the exponent size and significand size of
  values returned from most functions in this library. More concisely, it defines the significand
  size indirectly. The significand size corresponds to @racket[(bf-precision)] and is equal to
  @racket[(- (gfl-bits) (gfl-exponent))]. Default value is the length of a flonum in bits.
}

@defparam[gfl-rounding-mode rm (symbols 'nearest 'zero 'up 'down 'away)
          #:value 'nearest]{
  A parameter that determines the mode used to round the result of most functions in this library.
  Note that @racket[(bf-rounding-mode)] accepts all values except @racket['away].
}

@defparam[gfl-verbose? b boolean?
          #:value #t]{
  A parameter that controls the verbosity when printing generic-flonums. When set to true, generic-flonums
  are printed in a custom format: @racket["#<gfl[es, nb]: v>"]. When set to false, generic-flonums are
  printed in a format similar to bigfloats: @racket["(gfl v)"].
}

@section{Constants}

@deftogether[(@defthing[pi.gfl gfl?]
              @defthing[phi.gfl gfl?]
              @defthing[euler.gfl gfl?]
              @defthing[catalan.gfl gfl?]
              @defthing[log2.gfl gfl?])]{
  Approximations of π, φ, γ, G, and log(2).
}

@deftogether[(@defthing[+nan.gfl gfl?]
              @defthing[+inf.gfl gfl?]
              @defthing[+max.gfl gfl?]
              @defthing[+min.gfl gfl?]
              @defthing[0.gfl gfl?]
              @defthing[-0.gfl gfl?]
              @defthing[-min.gfl gfl?]
              @defthing[-max.gfl gfl?]
              @defthing[-inf.gfl gfl?])]{
 Constants corresponding to @racket[+nan.0], @racket[+inf.0], @racket[+max.0], @racket[+min.0],
 @racket[0.0], @racket[-0.0], @racket[-min.0], @racket[-max.0], and @racket[-inf.0].
}

@deftogether[(@defthing[10.gfl gfl?]
              @defthing[9.gfl gfl?]
              @defthing[8.gfl gfl?]
              @defthing[7.gfl gfl?]
              @defthing[6.gfl gfl?]
              @defthing[5.gfl gfl?]
              @defthing[4.gfl gfl?]
              @defthing[3.gfl gfl?]
              @defthing[2.gfl gfl?]
              @defthing[1.gfl gfl?]
              @defthing[-1.gfl gfl?]
              @defthing[-2.gfl gfl?]
              @defthing[-3.gfl gfl?]
              @defthing[-4.gfl gfl?]
              @defthing[-5.gfl gfl?]
              @defthing[-6.gfl gfl?]
              @defthing[-7.gfl gfl?]
              @defthing[-8.gfl gfl?]
              @defthing[-9.gfl gfl?]
              @defthing[-10.gfl gfl?])]{
  More constants.
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

  When giving more than two arguments, @racket[gfl+] and @racket[gfl-] compute the answer without
  any intermediate rounding.
}

@deftogether[(@defproc[(gflmax [x gfl?] ...) gfl?]
              @defproc[(gflmin [x gfl?] ...) gfl?])]{
  Returns the maximum and minimum of their arguments, respectively. When given no arguments, @racket[bfmin]
  returns @racket[+inf.gfl], and @racket[bfmax] returns @racket[-inf.gfl].
}

@deftogether[(@defproc[(gflmod [x gfl?] [y gfl?]) gfl?]
              @defproc[(gflremainder [x gfl?] [y gfl?]) gfl?])]{
  Returns the modulo and remainder of @racket[x] and @racket[y].
}

@defproc[(gfldim [x gfl?] [y gfl?])
          gfl?]{
  Returns @racket[(gfl- x y)] if @racket[(gfl> x y)], and @racket[0.gfl] otherwise. Equivalent to
  @racket[(gflmax (gfl- x y) 0.gfl)].
}

@defproc[(gflagm [x gfl?] [y gfl?])
          gfl?]{
  Returns the arithmetic-geometric mean of @racket[x] and @racket[y].
}

@defproc[(gflfma [x gfl?] [y gfl?] [z gfl?])
          gfl?]{
  Computes @racket[(gfl+ (gfl* x y) z)] without intermediate overflow or rounding.
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

@defproc[(gflsin+cos [x gfl?])
          (values gfl? gfl?)]{
  Simultaneously computes the sine and cosine of @racket[x].
}

@defproc[(gflsinh+cosh [x gfl?])
          (values gfl? gfl?)]{
  Simultaneously computes the hyperbolic sine and cosine of @racket[x].
}

@deftogether[(@defproc[(gflgamma [x gfl?]) gfl?]
              @defproc[(gfllgamma [x gfl?]) gfl?]
              @defproc[(gfldigamma [x gfl?]) gfl?])]{
  Compute the gamma, log-gamma, and digamma function.
}

@deftogether[(@defproc[(gflerf [x gfl?]) gfl?]
              @defproc[(gflerfc [x gfl?]) gfl?])]{
  Compute the error function and complementary error function.
}

@defproc[(gfleint [x gfl?])
          gfl?]{
  Returns the exponetial integral of @racket[x].
}

@defproc[(gflli2 [x gfl?])
          gfl?]{
  Returns the dilogarithm of @racket[x].
}

@defproc[(gflzeta [x gfl?])
          gfl?]{
  Computes the Riemann zeta function.
}

@deftogether[(@defproc[(gflj0 [x gfl?]) gfl?]
              @defproc[(gflj1 [x gfl?]) gfl?]
              @defproc[(gfljn [n exact-integer?] [x gfl?]) gfl?]
              @defproc[(gfly0 [x gfl?]) gfl?]
              @defproc[(gfly1 [x gfl?]) gfl?]
              @defproc[(gflyn [n exact-integer?] [x gfl?]) gfl?])]{
  Compute the Bessel functions. The first three correspond to Bessel functions of
  the first kind of order 0, 1, and @racket[n], while the other three correspond to
  Bessel functions of the second kind of order 0, 1, and @racket[n].
}

@defproc[(gflai [x gfl?])
          gfl?]{
  Computes the Airy function of the first kind.
}

@section{Rounding}

@deftogether[(@defproc[(gflceiling [x gfl?]) gfl?]
              @defproc[(gflfloor [x gfl?]) gfl?]
              @defproc[(gflround [x gfl?]) gfl?]
              @defproc[(gfltruncate [x gfl?]) gfl?])]{
  Like @racket[ceiling], @racket[floor], @racket[round], and @racket[truncate], but
  for generic-flonums.
}

@defproc[(gflrint [x gfl?])
          gfl?]{
  Rounds @racket[x] to the nearest integer in the direction specified by @racket[(gfl-rounding-mode)].
}

@section{Miscellaneous}

@defproc[(gflcopysign [x gfl?] [y gfl?])
          gfl?]{
  Returns a generic-flonum with the magnitude of @racket[x] and the sign of @racket[y].
}

@deftogether[(@defproc[(gfls-between [x gfl?] [y gfl?]) gfl?]
              @defproc[(gflnext [x gfl?]) gfl?]
              @defproc[(gflprev [x gfl?]) gfl?]
              @defproc[(gflstep [x gfl?] [n exact-integer?]) gfl?])]{
  Like @racket[flonums-between], @racket[flnext], @racket[flprev], and @racket[flstep], but for
  generic-flonums.
}