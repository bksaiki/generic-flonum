#lang racket

(require "gfl.rkt"
         (for-syntax racket/base racket/syntax syntax/strip-context))

;;;;;;;;;;;;;;;; Constants ;;;;;;;;;;;;;;;;

(define-for-syntax consts (list))

(define-syntax-rule (gfl-const name val)
  (begin 
    (define (name) (real->gfl val))
    (begin-for-syntax (set! consts (cons #'name consts)))))

(define-syntax-rule (gfl-consts [name val] ...)
  (begin (gfl-const name val) ...))

(gfl-consts
 [+inf.gfl +inf.0]
 [-inf.gfl -inf.0]
 [+nan.gfl +nan.0]
 [-10.gfl  -10.0]
 [-9.gfl   -9.0]
 [-8.gfl   -8.0]
 [-7.gfl   -7.0]
 [-6.gfl   -6.0]
 [-5.gfl   -5.0]
 [-4.gfl   -4.0]
 [-3.gfl   -3.0]
 [-2.gfl   -2.0]
 [-1.gfl   -1.0]
 [-0.gfl   -0.0]
 [0.gfl    0.0]
 [1.gfl    1.0]
 [2.gfl    2.0]
 [3.gfl    3.0]
 [4.gfl    4.0]
 [5.gfl    5.0]
 [6.gfl    6.0]
 [7.gfl    7.0]
 [8.gfl    8.0]
 [9.gfl    9.0]
 [10.gfl   10.0])

;; The following code is from racket/math
(define-syntax (req/prov-constants stx)
  (syntax-case stx ()
    [(_ collection force)
     (with-syntax ([require-it-name  (datum->syntax stx (gensym 'require-it))])
       (syntax/loc stx
         (begin
           (define-syntax (require-it-name stx1)
             (syntax-case stx1 ()
               [(require-it-name)
                (with-syntax* ([(name (... ...))  (replace-context #'require-it-name collection)]
                               [(stx-name (... ...))  (map (λ (name) (format-id name "stx:~a" name))
                                                           (syntax->list #'(name (... ...))))])
                  #'(begin (define-syntax (stx-name stx)
                             (syntax-case stx ()
                               [(_ . args)  (syntax/loc stx ((force name) . args))]
                               [_  (syntax/loc stx (force name))]))
                           (... ...)
                           (provide (rename-out [stx-name name] (... ...)))))]))
           (require-it-name))))]))

(define-syntax-rule (apply0 x) (x))

(req/prov-constants consts apply0)
(req/prov-constants const-funs apply0)