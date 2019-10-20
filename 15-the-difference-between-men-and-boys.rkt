#lang racket
(require "utils.rkt")
(provide (all-defined-out))
(require racket/trace)
(require rackunit)

;
; # Note
;
;

(define x '(chicago pizza))

(set! x 'gone)

(set! x 'skins)

(define gourmet
  (λ (food)
    (list food x)))

(check-equal? (gourmet 'onion) '(onion skins))

(set! x 'rings)

(check-equal? (gourmet 'onion) '(onion rings))

(define gourmand
  (λ (food)
    (set! x food)
    (list food x)))

(check-equal? (gourmand 'potato) '(potato potato))

(check-equal? (gourmand 'rice) '(rice rice))

(define dinerR
  (λ (food)
    (set! x food)
    (list 'milkshake food)))

(check-equal? (dinerR 'onion) '(milkshake onion))
(check-equal? (dinerR 'pecanpie) '(milkshake pecanpie))
(check-equal? (dinerR 'onion) '(milkshake onion))

(define omnivore
  (let ([x 'minestrone])
    (λ (food)
      (set! x food)
      (list food x))))

(check-equal? (omnivore 'bouillabaisse) '(bouillabaisse bouillabaisse))

;
; ## The 16th Commandment
;
; > Use `(set! ...)` only with names defined in `(let ...)`s.
;

(define gobbler
  (let([x 'minestrone])
    (λ (food)
      (set! x food)
      (list food x))))

(check-equal? (gobbler 'gumbo) '(gumbo gumbo))

;
; ## The 17th Commandment
;
; > Use `(set! ...)` for `(let ([x ...]) ...)` only if
; > there is at least one `(λ ...` between it and the
; > `(let ([x ...]) ...)`
;

(define food 'none)

(define glutton
  (λ (x)
    (set! food x)
    (list 'more x 'more x)))

(check-equal? (glutton 'garlic) '(more garlic more garlic))
(check-equal? food 'garlic)
(check-equal? x 'onion)

(define chez-nous
  (λ ()
    (set! food x)
    (set! x food)))

(check-equal? food 'garlic)
(check-equal? x 'onion)

(chez-nous)

(check-equal? food 'onion)
(check-equal? x 'onion)

;
; ## The 18th Commandment
;
; > Use `(set! x ...)` only when the value that x refers to
; > is no longer needed.
;

(define chez-nous-fixed
  (λ ()
    (let ([a food])
      (set! food x)
      (set! x a))))

(check-equal? (glutton 'garlic) '(more garlic more garlic))
(check-equal? food 'garlic)
(check-equal? (gourmand 'potato) '(potato potato))
(check-equal? x 'potato)

(chez-nous-fixed)

(check-equal? food 'potato)
(check-equal? x 'garlic)
