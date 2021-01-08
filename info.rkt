#lang info

(define name "generic-flonum")
(define deps '("math-lib" "base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/generic-flonum.scrbl")))
(define pkg-desc "Generic IEEE-754 floating-point numbers")
(define version "1.0")
(define pkg-authors '("Brett Saiki"))