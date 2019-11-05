#lang racket
(require "utils.rkt")
(require racket/trace)
(require rackunit)

(define deep
  (λ (m)
    (cond [(zero? m) 'pizza]
          [else (list (deep (sub1 m)))])))

(check-equal? (deep 6) '((((((pizza)))))))
; || > (deep 5)
; || > >(deep 4)
; || > > (deep 3)
; || > > >(deep 2)
; || > > > (deep 1)
; || > > > >(deep 0)
; || < < < <'pizza    ; same as (six-layers p)
; || < < < '(pizza)
; || < < <'((pizza))
; || < < '(((pizza)))
; || < <'((((pizza))))
; || < '(((((pizza)))))
; || <'((((((pizza))))))

(define six-layers
  (λ (p)
    (list
      (list
        (list
          (list
            (list
              (list p))))))))

(define four-layers
  (λ (p)
    (list
      (list
        (list
          (list p))))))

(check-equal? (six-layers 'pizza) '((((((pizza)))))))
(check-equal? (four-layers 'pizza) '((((pizza)))))

(define toppings #f) ; init

(define deepB
  (λ (m)
    (cond
      [(zero? m)
       (let/cc jump
         (set! toppings jump) ; save the context `jump`
         'pizza)]
      [else (list (deepB (sub1 m)))])))

(println "(deepB 6):")
(newline)

(deepB 6)

(toppings 'mozzarella)
(toppings 'cake)
(toppings 'pizza)
(cons (toppings 'cake) '()) ; forgets cons
(cons
  (cons
    (cons
      (toppings 'cake)      ; forgets cons
      '())
    '())
  '())
(newline)

(println "(deepB 4):")
(newline)

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

; --------------------------------------------------------- *
; The 12th Commandment                                      ;
;                                                           ;
; When thinking about a value crated with (let/cc ...),     ;
; write down the function that is equivalent but does not   ;
; for get. Then, when you use it, remember to forget.       ;
; --------------------------------------------------------- *

; deep & continuation

(define deep&co
  (λ (m k)
    (cond
      [(zero? m) (k 'pizza)]
      [else (deep&co (sub1 m)
                     (λ (x) (k (list x))))])))
; || >(deep&co 2 #<procedure:...h-the-jewels.rkt:117:25>)
; || >(deep&co 1 #<procedure:...h-the-jewels.rkt:110:21>)
; || >(deep&co 0 #<procedure:...h-the-jewels.rkt:110:21>)
; || <'((pizza))

(define two-layers
  (letrec ([k (λ (x) (k1 (list x)))]
           [k1 (λ (x) (k2 (list x)))]
           [k2 (λ (x) x)])
    k))

(check-equal? (deep&co 2 (λ (x) x)) '((pizza)))
(check-equal? (two-layers 'pizza) '((pizza)))

; deep & continuation with

(set! toppings #f) ; init

(define deep&coB
  (λ (m k)
    (cond
      [(zero? m) (let ()
                   (set! toppings k) ; save the continuation
                   (k 'pizza))]
      [else (deep&coB (sub1 m)
                      (λ (x) (k (list x))))])))
; || >(deep&coB 4 #<procedure:...h-the-jewels.rkt:133:12>)
; || >(deep&coB 3 #<procedure:...h-the-jewels.rkt:129:22>)
; || >(deep&coB 2 #<procedure:...h-the-jewels.rkt:129:22>)
; || >(deep&coB 1 #<procedure:...h-the-jewels.rkt:129:22>)
; || >(deep&coB 0 #<procedure:...h-the-jewels.rkt:129:22>)
; || <'((((pizza))))

(check-equal? (deep&coB 6 (λ (x) x)) '((((((pizza)))))))
(check-equal? (deep&coB 4 (λ (x) x)) '((((pizza)))))

(check-equal?  (cons (toppings 'cake) (toppings 'cake))
               (cons '((((cake)))) '((((cake))))))

(check-equal?  (cons (toppings 'cake)
                     (cons (toppings 'mozzarella)
                           (cons (toppings 'pizza)
                                 '())))
               '(((((cake)))) ((((mozzarella)))) ((((pizza))))))

; two-in-a-row

((λ ()

   (define two-in-a-row?
     (λ (lat)
       (match lat
         [`() #f]
         [`(,x ,_lat ...) (two-in-a-row-b? x _lat)])))

   (define two-in-a-row-b?
     (λ (a lat)
       (match lat
         [`() #f]
         [`(,x ,_lat ...) (if (eq? a x)
                              #t
                              (two-in-a-row-b? x _lat))])))

   (check-equal? (two-in-a-row? '(Italian sardines spaghetti parsley)) #f)
   (check-equal? (two-in-a-row? '(Italian sardines sardines spaghetti parsley)) #t)
   (check-equal? (two-in-a-row? '(Italian sardines more spaghetti parsley)) #f)
   (check-equal? (two-in-a-row? '(b d e i i a g)) #t)

   ))

((λ () ; use letrec

   (define two-in-a-row?
     (letrec
       ([W (λ (a lat)
             (match lat
               [`() #f]
               [`(,x ,_lat ...) (if (eq? a x)
                                    #t
                                    (W x _lat))]))])
       (λ (lat)
         (match lat
           [`() #f]
           [`(,x ,_lat ...) (W x _lat)]))))

   (check-equal? (two-in-a-row? '(Italian sardines spaghetti parsley)) #f)
   (check-equal? (two-in-a-row? '(Italian sardines sardines spaghetti parsley)) #t)
   (check-equal? (two-in-a-row? '(Italian sardines more spaghetti parsley)) #f)
   (check-equal? (two-in-a-row? '(b d e i i a g)) #t)

   ))

; walk - crawls over list from left to right until it finds an atom
;        and then use `leave` to return that atom.

(define leave #f) ; init

(define walk
  (λ (l)
    (match l
      [`() '()]
      [`(,(? atom? x) ,_l ...) (leave x)]
      [`(,s ,_l ...) (let ()
                      (walk s)
                      (walk _l))])))

(define start-it
  (λ (l)
    (let/cc here
      (set! leave here)
      (walk l))))

(println "(walk)")
(start-it '((potato) (chips (chips (with))) fish))
(newline)

; waddle

(define LEAVE #f) ; init
(define FILL #f) ; init

(define waddle
  (λ (l)
    (match l
      [`() '()]
      [`(,(? atom? x) ,_l ...) (let ()
                                 (let/cc rest
                                   (set! FILL rest) ; FILL what is left to do
                                   (LEAVE x))
                                 (waddle _l))]
      [`(,s ,_l ...) (let ()
                       (waddle s)
                       (waddle _l))])))

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

(newline)

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
