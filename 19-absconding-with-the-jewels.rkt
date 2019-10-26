#lang racket
(require "utils.rkt")
(provide (all-defined-out))
(require racket/trace)
(require rackunit)

(define deep
  (λ (m)
    (cond [(zero? m) 'pizza]
          [else (list (deep (sub1 m)))])))

(check-equal? (deep 6) '((((((pizza)))))))

(define toppings #f)
(define deepB
  (λ (m)
    (cond [(zero? m) (let/cc jump
                       (set! toppings jump) ; remember the context `jump`
                       'pizza)]
          [else (list (deepB (sub1 m)))])))

(println "(deepB 6)")

(deepB 6)
(toppings 'mozzarella)
(toppings 'cake)
(toppings 'pizza)
(cons (toppings 'cake) '())
(cons
  (cons
    (cons
      (toppings 'cake)
      '())
    '())
  '())

(newline)

(println "(deepB 4)")
(deepB 4)
(cons
  (cons
    (cons
      (toppings 'cake)
      '())
    '())
  '())
(cons
  (toppings 'cake)
  (toppings 'cake))
(cons (toppings 'cake)
      (cons (toppings 'mozzarella)
            (cons (toppings 'pizza)
                  '())))
(newline)

(define deep&co
  (λ (m kk)
    (cond [(zero? m) (kk 'pizza)]
          [else (deep&co (sub1 m)
                         (λ (x) (kk (list x))))])))

(println "(deep&co 2 (λ (x) x))")
(trace deep&co)
(deep&co 2 (λ (x) x))
(newline)

(define deep&coB
  (λ (m kk)
    (cond [(zero? m) (let ()
                       (set! toppings kk) ; save the continuation
                       (kk 'pizza))]
          [else (deep&coB (sub1 m)
                         (λ (x) (kk (list x))))])))

(println "(deep&coB 4 (λ (x) x))")
(deep&coB 4 (λ (x) x))
(cons
  (toppings 'cake)
  (toppings 'cake))
(cons (toppings 'cake)
      (cons (toppings 'mozzarella)
            (cons (toppings 'pizza)
                  '())))
(newline)

;

#| (define two-in-a-row? |#
#|   (letrec |#
#|     ([W (λ (a lat) |#
#|           (cond [(null? lat) #f] |#
#|                 [else (let ([next (car lat)]) |#
#|                         (if (eq? next a) |#
#|                             #t |#
#|                             (W next (cdr lat))))]))]) |#
#|     (λ (lat) |#
#|       (cond [(null? lat) #f] |#
#|             [else (W (car lat) (cdr lat))])))) |#

(define leave (λ (x) x))

(define walk
  (λ (l)
    (match l
      [`() '()]
      [`(,(? atom? x) ,y ...) (leave x)]
      [`(,x ,y ...) (let ()
                      (walk x)
                      (walk y))])))

(define start-it
  (λ (l)
    (let/cc here
      (set! leave here)
      (walk l))))

(println "(walk)")
(start-it '((potato) (chips (chips (with))) fish))
(newline)

;

(define LEAVE #f)
(define FILL #f)

(define waddle
  (λ (l)
    (match l
      [`() '()]
      [`(,(? atom? x) ,y ...) (let ()
                                (let/cc rest
                                  (set! FILL rest) ; FILL what is left to do
                                  (LEAVE x))
                                (waddle y))]
      [`(,x ,y ...) (let ()
                      (waddle x)
                      (waddle y))])))

(set! start-it
  (λ (l)
    (let/cc here
      (set! LEAVE here)
      (waddle l))))

(define get-next
  (λ (x)
    (let/cc here-again
      (set! LEAVE here-again)
      (FILL 'go))))

(println "(waddle)")

(start-it '((donuts)
             (cheerios (cheerios (spaghettios)))
             donuts)) ; donuts
(get-next 'go)        ; cheerios
(get-next 'go)        ; cheerios
(get-next 'go)        ; spaghettios
(get-next 'go)        ; donuts
(get-next 'go)        ; '()
(get-next 'go)        ; '()

(newline)

(define get-first
  (λ (l)
    (let/cc here
      (set! LEAVE here)
      (waddle l)
      (LEAVE '()))))

(println "(get-first)")
(newline)

(get-first '()) ; '()
(get-next 'go)  ; '()
(newline)

(get-first '(donut)) ; donuts
(get-next 'go)       ; '()
(newline)

(get-first '(fish (chips))) ; fish
(get-next 'go)              ; chips
(get-next 'go)              ; '()
(newline)

(get-first '(fish (chips) chips)) ; fish
(get-next 'go)                    ; chips
(get-next 'go)                    ; chips
(get-next 'go)                    ; '()
(newline)

;

(define two-in-a-row*?
  (letrec
    ([T? (λ (a)
           (let ([n (get-next 0)])
             (if (atom? n)
                 (or (eq? n a)
                     (T? n))
                 #f)))])
    (λ (l)
      (let ([fst (get-first l)])
        (if (atom? fst)
            (T? fst)
            #f)))))

(two-in-a-row*? '((mozzarella) (cake) mozzarella))         ; #f
(two-in-a-row*? '((potato) (chips ((with) fish) (fish))))  ; #t
(two-in-a-row*? '((potato) (chips ((with) fish) (chips)))) ; #f
(two-in-a-row*? '((potato) (chips (chips (with) fish))))   ; #t
(two-in-a-row*? '(((food) ()) (((food)))))                 ; #t
