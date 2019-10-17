#lang racket
(provide (all-defined-out))

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define (s-exp? x)
  (or (atom? x)
      (list? x)))

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
