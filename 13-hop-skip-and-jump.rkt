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

; intersectall :: List-of-Set -> Set
(define (intersectall lset)
  (let/cc hop
    (letrec ([A (λ (lset)
                  (match lset
                    [`(() ,_ ...) (hop "empty!")] ; [(null? (car lset)) ...]
                    [`(,y) y]                     ; [(null? (cdr lset)) ...]
                    [`(,x ,y ...) (I x (A y))]))] ; [else ...]
             [I (λ (s1 s2)
                  (letrec ([M? (λ (x) (list? (member x s2)))]
                           [J (λ (set)
                                (match set
                                  [`() '()]                             ; [(null? s1) '()]
                                  [`(,(? M? x) ,y ...) (list* x (J y))] ; [(member? (car s1) s2) ...]
                                  [`(,x ,y ...) (J y)]))])              ; [else ...]
                    (match s2
                      [`() (hop "empty!!")]
                      [else (J s1)])))])
      (match lset
        [`() '()]           ; [(null? lset) ...]
        [else (A lset)])))) ; [else ...]

;
(define rember
  (λ (a lat)
    (letrec ([A? (λ (x) (eq? x a))]
             [R (λ (lat)
                  (match lat
                    [`() '()]
                    [`(,(? A? x) ,y ...) y]
                    [`(,x ,y ...) (list* x (R y))]))])
      (R lat))))

;
(define (rember-beyond-first a lat)
  (letrec ([A? (λ (x) (eq? x a))]
           [R (λ (lat)
                (match lat
                  [`() '()]
                  [`(,(? A? x) ,y ...) '()]
                  [`(,x ,y ...) (list* x (R y))]))])
    (R lat)))

;
(define (rember-upto-last a lat)
  (let/cc skip
    (letrec ([A? (λ (x) (eq? x a))]
             [R (λ (lat)
                  (match lat
                    [`() '()]
                    [`(,(? A? x) ,y ...) (skip (R y))]
                    [`(,x ,y ...) (list* x (R y))]))])
      #| (trace R) |#
      (R lat))))

