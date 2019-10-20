#lang racket
(require "utils.rkt")
(provide (all-defined-out))
(require racket/trace)

;
; # Note
;
; ## Local Bindings and Continuations
;
; 1. `let`
; 2. `let*`
; 3. `letrec`
; 4. `let/cc`
;

(define (leftmost l)
  (let/cc skip
    (letrec ([LM (λ (l)
                  (match l
                    [`() '()]
                    [`(,(? atom? x) ,y ...) (skip x)]
                    [`(,x ,y ...) (let ()
                                    (LM x)
                                    (LM y))]))])
      (LM l))))

; TODO: (try x a b)
; via (define-syntax) ;
; http://www.greghendershott.com/fear-of-macros/Transform_.html#%28part._.What_is_a_syntax_transformer_%29
(define (rember1* a l)
  (letrec ([A? (λ (x) (eq? x a))]
           [RM (λ (l oh)
                 (match l
                   [`() (oh 'NO)]
                   [`(,(? atom? x) ,y ...) (if (A? x)
                                               y
                                               (list* x (RM y oh)))]
                   [`(,x ,y ...) (let ([new-x (let/cc oh (RM x oh))])
                                   (if (atom? new-x)
                                       (list* x (RM y oh))
                                       (list* new-x y)))]))])
    #| (trace RM) |#
    (let ([new-l (let/cc oh (RM l oh))])
      (if (atom? new-l)
          l
          new-l))))

#| (define (rember1* a l) |#
#|   (letrec ([A? (λ (x) (eq? x a))] |#
#|            [R (λ (l) |#
#|                 (match l |#
#|                   [`() '()] |#
#|                   [`(,(? atom? x) ,y ...) (if (A? x) |#
#|                                               y |#
#|                                               (list* x (R y)))] |#
#|                   [`(,x ,y ...) (let ([av (R x)]) |#
#|                                   (if (equal? av x) |#
#|                                       (list* x (R y)) |#
#|                                       (list* av y)))]))]) |#
#|     (R l))) |#

(define (depth* l)
  (match l
    [`() 1]
    [`(,(? atom? x) ,y ...) (depth* y)]
    [`(,x ,y ...) (max (add1 (depth* x))
                       (depth* y))]))
