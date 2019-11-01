#lang racket
(require "utils.rkt")
(provide (all-defined-out))
(require racket/trace)
(require rackunit)

(define sweet-tooth
  (λ (food)
    (list food 'cake)))

(define last 'angelfood)

(check-equal? (sweet-tooth 'chocolate) '(chocolate cake))
(check-equal? last 'angelfood)

(check-equal? (sweet-tooth 'fruit) '(fruit cake))
(check-equal? last 'angelfood)

; sweet-tooth `last`

(define sweet-toothL
  (λ (food)
    (set! last food)
    (list food 'cake)))

(check-equal? (sweet-toothL 'chocolate) '(chocolate cake))
(check-equal? last 'chocolate)

(check-equal? (sweet-toothL 'fruit) '(fruit cake))
(check-equal? last 'fruit)

(check-equal? (sweet-toothL 'cheese) '(cheese cake))

(check-equal? (sweet-toothL 'carrot) '(carrot cake))

; sweet-tooth `remember`

(define ingredients '())

(define sweet-toothR
  (λ (food)
    (set! ingredients (cons food ingredients))
    (list food 'cake)))

(check-equal? (sweet-toothR 'chocolate) '(chocolate cake))
(check-equal? ingredients '(chocolate))

(check-equal? (sweet-toothR 'fruit) '(fruit cake))
(check-equal? ingredients '(fruit chocolate))

(check-equal? (sweet-toothR 'cheese) '(cheese cake))
(check-equal? ingredients '(cheese fruit chocolate))

(check-equal? (sweet-toothR 'carrot) '(carrot cake))
(check-equal? ingredients '(carrot cheese fruit chocolate))

;

(define deep
  (λ (m)
    (cond [(zero? m) 'pizza]
          [else (list (deep (sub1 m)))])))

(check-equal? (deep 3) '(((pizza))))
(check-equal? (deep 7) '(((((((pizza))))))))
(check-equal? (deep 0) 'pizza)

; deep `rembmer`

(define Ns '()) ; `n`s

(define Rs '()) ; `R`esults

(define deepR
  (λ (n)
    (let ([result (deep n)])
      (set! Rs (cons result Rs))
      (set! Ns (cons n Ns))
      result)))

(check-equal? (deepR 3) '(((pizza))))
(check-equal? Ns '(3))
(check-equal? Rs '((((pizza)))))

(check-equal? (deepR 5) '(((((pizza))))))
(check-equal? Ns '(5 3))
(check-equal? Rs '((((((pizza))))) (((pizza)))))

(check-equal? (deepR 3) '(((pizza))))
(check-equal? Ns '(3 5 3))
(check-equal? Rs '((((pizza))) (((((pizza))))) (((pizza)))))

;

(define find
  (λ (n Ns Rs)
    (letrec ([A (λ (ns rs)
                  (cond [(= (car ns) n) (car rs)]
                        [else (A (cdr ns) (cdr rs))]))])
      (A Ns Rs))))

(check-equal? (find 3 Ns Rs) '(((pizza))))
(check-equal? (find 5 Ns Rs) '(((((pizza))))))

; deep `memorized`

(define deepM
  (λ (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (deepR n))))

(check-equal? Ns '(3 5 3))
(check-equal? Rs '(
                   (((pizza)))
                   (((((pizza)))))
                   (((pizza)))
                   ))

(set! Ns (cdr Ns))
(set! Rs (cdr Rs))

(check-equal? Ns '(5 3))
(check-equal? Rs '(
                   (((((pizza)))))
                   (((pizza)))
                   ))

; mutual recursions of deepM & deep

(set! deepM
  (λ (n)
    (if (member? n Ns)
        (find n Ns Rs)
        (let ([result (deep n)])
          (set! Rs (cons result Rs))
          (set! Ns (cons n Ns))
          result))))

(check-equal? (deepM 6) '((((((pizza)))))))

(set! deep
  (λ (m)
    (cond [(zero? m) 'pizza]
          [else (list (deepM (sub1 m)))])))

(check-equal? (deepM 9) '(((((((((pizza))))))))))
(check-equal? Ns '(9 8 7 6 5 3))

; fix deepM by the 16th Commandment

(set! deepM
  (let ([Rs '()]
        [Ns '()])
    (λ (n)
      (if (member? n Ns)
          (find n Ns Rs)
          (let ([result (deep n)])
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)))))

(check-equal? (deepM 16) '((((((((((((((((pizza)))))))))))))))))

(set! find
  (λ (n Ns Rs)
    (letrec ([A (λ (ns rs)
                  (cond [(null? ns) #f]
                        [(= (car ns) n) (car rs)]
                        [else (A (cdr ns) (cdr rs))]))])
      (A Ns Rs))))

(set! deepM
  (let ([Rs '()]
        [Ns '()])
    (λ (n)
      (let ([exists (find n Ns Rs)])
        (if (atom? exists)
            (let ([result (deep n)])
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

; length

(define length
  (λ (l)
    (match l
      [`() 0]
      [`(,x ,_l ...) (add1 (length _l))])))

(check-equal? (length '(1 2 3 4 5 6 7)) 7)

(set! length
  (let ([h #f])
    (set! h
      (λ (l)
        (match l
          [`() 0]
          [`(,x ,_l ...) (add1 (h _l))])))
    h))

(check-equal? (length '(1 2 3 4 5 6 7)) 7)

;
; ## The 17th Commandment (final ver.)
;
; > Use `(set! x ...) for `(let ([x ...]) ...)` only if there is
; > at least one `(λ ...` between it and the `(let ...)`, or if
; > the new value for `x` is afunction that refers to `x`.
;

(define L
  (λ (length)
    (λ (l)
      (match l
        [`() 0]
        [`(,x ,_l ...) (add1 (length _l))]))))

(set! length
  (let ([h #f])
    (set! h
      (L (λ (x) (h x))))
    h))

(check-equal? (length '(1 2 3 4 5 6 7)) 7)

; Y! and Y-bang

(define Y!
  (λ (L)
    (let ([h #f])
      (set! h
        (L (λ (x) (h x))))
      h)))

(define Y-bang
  (λ (f)
    (letrec
      ([h (f (λ (x) (h x)))])
      h)))

; define length by Y!

(set! length (Y! L))
(check-equal? (length '(1 2 3 4 5 6 7)) 7)

; define length by Y-bang

(set! length (Y-bang L))
(check-equal? (length '(1 2 3 4 5 6 7)) 7)

;

(define D
  (λ (depth*)
    (λ (s)
      (cond [(null? s) 1]
            [(atom? (car s)) (depth* (cdr s))]
            [else (max (add1 (depth* (car s)))
                       (depth* (cdr s)))]))))


