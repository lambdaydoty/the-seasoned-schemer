#lang racket
(require (for-syntax racket/match))
(provide (all-defined-out))

(define atom?
  (λ (x)
    (and (not (pair? x))
         (not (null? x)))))

(define s-exp?
  (λ (x)
    (or (atom? x)
        (list? x))))

(define (listing . l)
  (begin
    (for-each (lambda (x) (printf "~a " x)) l)
    (newline)
    ))

(define (map fn l)
  (cond [(null? l) '()]
        [else (cons (fn (car l))
                    (map fn (cdr l)))]))

(define (filter p l)
  (cond [(null? l) '()]
        [else (if (p (car l))
                  (cons (car l) (filter p (cdr l)))
                  (filter p (cdr l)))]))

(define Y
  (λ (f)
    (λ (g)
      (((λ (x) (f (λ (y) ((x x) y))))
        (λ (x) (f (λ (y) ((x x) y)))))
       g))))

(define one?
  (λ (n) (zero? (sub1 n))))

(define member?
  (λ (x l)
    (list? (member x l))))

;
; http://www.greghendershott.com/fear-of-macros/Transform_.html
;
; (try continuation tryer catcher)
;
(define-syntax (try stx)
  (match (syntax->list stx)
    [`(,_ ,cont ,tryer ,catcher)
      (datum->syntax stx `(let/cc success
                            (let/cc ,cont
                              (success ,tryer))
                            ,catcher))]))
