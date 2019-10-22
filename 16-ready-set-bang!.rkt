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

;;

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

;;

(define deep
  (λ (m)
    (cond [(zero? m) 'pizza]
          [else (list (deep (sub1 m)))])))

(check-equal? (deep 3) '(((pizza))))
(check-equal? (deep 7) '(((((((pizza))))))))
(check-equal? (deep 0) 'pizza)

(define Ns '())

(define Rs '())

(define deepR
  (λ (n)
    (let ([result (deep n)])
      (set! Rs (cons result Rs))
      (set! Ns (cons n Ns))
      result)))

(check-equal? (deepR 3) '(((pizza))))
(check-equal? (deepR 5) '(((((pizza))))))
(check-equal? (deepR 3) '(((pizza))))

;
; ## The 19th Commandment
;
; > Use `(set! ...) to remember valuable things between two distinct
; > uses of a function
;

(define find
  (λ (n Ns Rs)
    (letrec ([A (λ (ns rs)
                  (cond [(= (car ns) n) (car rs)]
                        [else (A (cdr ns) (cdr rs))]))])
      (A Ns Rs))))

(check-equal? (find 3 Ns Rs) '(((pizza))))
(check-equal? (find 5 Ns Rs) '(((((pizza))))))

;

(define (member? x l) (list? (member x l)))

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

; Fix deepM by the 16th Commandment

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

; Y!

(define length0
  (λ (l)
    (cond [(null? l) 0]
          [else (add1 (length0 (cdr l)))])))

(define length1
  (λ (l) 0))

(set! length1
  (λ (l)
    (cond [(null? l) 0]
          [else (add1 (length1 (cdr l)))])))

(define length2
  (let ([h (λ (l) 0)])
    (set! h (λ (l)
              (cond [(null? l) 0]
                    [else (add1 (h (cdr l)))])))
    h))

(check-equal? (length0 '(1 2 3 4 5 6 7)) 7)
(check-equal? (length1 '(1 2 3 4 5 6 7)) 7)
(check-equal? (length2 '(1 2 3 4 5 6 7)) 7)

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
      (cond [(null? l) 0]
            [else (add1 (length (cdr l)))]))))

((λ ()
   (define length
     (let ([h (λ (l) 0)])
       (set! h (L (λ (x) (h x))))
       h))
   (check-equal? (length '(1 2 3 4 5 6 7)) 7)))

(define Y!
  (λ (L)
    (let ([h (λ (l) '())])
      (set! h (L (λ (x) (h x))))
      h)))

(define Y-bang
  (λ (f)
    (letrec ([h (f (λ (x) (h x)))])
      h)))

((λ ()
   ; define (length l) by Y!
   (define length (Y! L))
   (check-equal? (length '(1 2 3 4 5 6 7)) 7)))

((λ ()
   ; define (length l) by Y-bang
   (define length (Y-bang L))
   (check-equal? (length '(1 2 3 4 5 6 7)) 7)))

(define D
  (λ (depth*)
    (λ (s)
      (cond [(null? s) 1]
            [(atom? (car s)) (depth* (cdr s))]
            [else (max (add1 (depth* (car s)))
                       (depth* (cdr s)))]))))


