#lang racket
(require "utils.rkt")
(require rackunit)
(require racket/trace)

;
; Note: Local Bindings:
;       1) let
;       2) let*
;       3) letrec
;

((λ () ; multirember through Y

   (define multirember
     (λ (a lat)
       ((Y (λ (mr)
             (λ (lat)
               (match lat
                 [`() '()]
                 [`(,x ,_lat ...) (if (eq? a x)
                                      (multirember a _lat)
                                      (list* x (multirember a _lat)))]))))
        lat)))

   (check-equal? (multirember 'tuna '(shrimp salad tuna salad and tuna)) '(shrimp salad salad and))
   (check-equal? (multirember 'pie '(apple custard pie linzer pie torte)) '(apple custard linzer torte))
   (check-equal? (multirember 'i '(b d e i i a g)) '(b d e a g))

   ))

((λ () ; multirember through letrec

   (define multirember
     (λ (a lat)
       ((letrec ([MR (λ (lat)
                       (match lat
                         [`() '()]
                         [`(,x ,_lat ...) (if (eq? a x)
                                              (MR _lat)
                                              (list* x (MR _lat)))]))])
          MR)
        lat)))

   (check-equal? (multirember 'tuna '(shrimp salad tuna salad and tuna)) '(shrimp salad salad and))
   (check-equal? (multirember 'pie '(apple custard pie linzer pie torte)) '(apple custard linzer torte))
   (check-equal? (multirember 'i '(b d e i i a g)) '(b d e a g))

   ))

((λ () ; multirember through letrec, applicative style: (letrec ([MR ...]) (MR lat))

   (define multirember
     (λ (a lat)
       (letrec ([MR (λ (lat)
                      (match lat
                        [`() '()]
                        [`(,x ,_lat ...) (if (eq? a x)
                                             (MR _lat)
                                             (list* x (MR _lat)))]))])
         (MR lat))))

   (check-equal? (multirember 'tuna '(shrimp salad tuna salad and tuna)) '(shrimp salad salad and))
   (check-equal? (multirember 'pie '(apple custard pie linzer pie torte)) '(apple custard linzer torte))
   (check-equal? (multirember 'i '(b d e i i a g)) '(b d e a g))

   ))

;
; The 12th Commandment
;
; Use (letrec ...) to remove arguments that do not change for recursive applications.
;

((λ () ; multirember through test? function

   (define multirember-f
     (λ (test?)
       (letrec ([m-f (λ (a lat)
                       (match lat
                         [`() '()]
                         [`(,x ,_lat ...) (if (test? a x)
                                              (m-f a _lat)
                                              (list* x (m-f a _lat)))]))])
          m-f)))

   (define multirember
     (multirember-f eq?))

   (check-equal? (multirember 'tuna '(shrimp salad tuna salad and tuna)) '(shrimp salad salad and))
   (check-equal? (multirember 'pie '(apple custard pie linzer pie torte)) '(apple custard linzer torte))
   (check-equal? (multirember 'i '(b d e i i a g)) '(b d e a g))

   ))

((λ () ; member?

   (define member?
     (λ (a lat)
       (match lat
         [`() #f]
         [`(,x ,_lat ...) (if (eq? a x)
                              #t
                              (member? a _lat))])))

   (check-equal? (member? 'ice
                          '(salad greens with pears brie cheese frozen yogurt))
                 #f)
   ))

((λ () ; member? through letrec

   (define member?
     (λ (a lat)
       (letrec ([MEM? (λ (lat)
                     (match lat
                       [`() #f]
                       [`(,x ,_lat ...) (if (eq? a x)
                                            #t
                                            (MEM? _lat))]))])
         (MEM? lat))))

   (check-equal? (member? 'ice
                          '(salad greens with pears brie cheese frozen yogurt))
                 #f)
   ))

((λ () ; union

   (define union
     (λ (set1 set2)
       (match set1
         [`() set2]
         [`(,x ,_set ...) (if (member? x set2)
                              (union _set set2)
                              (list* x (union _set set2)))])))


   (check-equal? (union '(tomatoes and macaroni casserole)
                        '(macaroni and cheese))
                 '(tomatoes casserole macaroni and cheese))

   ))

((λ () ; union through letrec

   (define union
     (λ (set1 set2)
       (letrec ([U (λ (set)
                     (match set
                       [`() set2]
                       [`(,x ,_set ...) (if (member? x set2)
                                            (U _set)
                                            (list* x (U _set)))]))])
         (U set1))))

   (check-equal? (union '(tomatoes and macaroni casserole)
                        '(macaroni and cheese))
                 '(tomatoes casserole macaroni and cheese))

   ))

((λ () ; union modulized

   (define union
     (λ (set1 set2)
       (letrec ([U (λ (set)
                     (match set
                       [`() set2]
                       [`(,x ,_set ...) (if (R? x set2)
                                            (U _set)
                                            (list* x (U _set)))]))]
                [R? (λ (a lat)
                      (letrec ([MEM? (λ (lat)
                                       (match lat
                                         [`() #f]
                                         [`(,x ,_lat ...) (if (eq? a x)
                                                              #t
                                                              (MEM? _lat))]))])
                        (MEM? lat)))])
         (U set1))))

   (check-equal? (union '(tomatoes and macaroni casserole)
                        '(macaroni and cheese))
                 '(tomatoes casserole macaroni and cheese))

   ))

((λ () ; two-in-a-row? (b-version)

   (define two-in-a-row?
     (letrec ([W? (λ (preceding lat)
                   (match lat
                     [`() #f]
                     [`(,x ,_lat ...) (if (eq? preceding x)
                                          #t
                                          (W? x _lat))]))])
       (λ (lat)
         (match lat
           [`() #f]
           [`(,x ,_lat ...) (W? x _lat)]))))

   (check-equal? (two-in-a-row? '(Italian sardines spaghetti parsley)) #f)
   (check-equal? (two-in-a-row? '(Italian sardines sardines spaghetti parsley)) #t)
   (check-equal? (two-in-a-row? '(Italian sardines more spaghetti parsley)) #f)
   (check-equal? (two-in-a-row? '(b d e i i a g)) #t)

   ))

((λ () ; sum-of-prefixes

   (define sum-of-prefixes
     (λ (tup)
       (letrec ([S (λ (sonssf tup)
                     (match tup
                       [`() '()]
                       [`(,x ,_tup ...) (let ([sum (+ sonssf x)])
                                          (list* sum
                                                 (S sum _tup)))]))])
         (S 0 tup))))

   (check-equal? (sum-of-prefixes '(2 1 9 17 0)) '(2 3 12 29 29))
   (check-equal? (sum-of-prefixes '(1 1 1 1 1)) '(1 2 3 4 5))

   ))

((λ () ; scramble

   (define pick
     (λ (n lat)
       (cond [(one? n) (car lat)]
             [else (pick (sub1 n)
                         (cdr lat))])))

   (define scramble
     (λ (tup)
       (letrec ([P (λ (tup rev-pre)
                     (match tup
                       [`() '()]
                       [`(,x ,_tup ...) (let ([new (list* x rev-pre)])
                                          (list* (pick x new)
                                                 (P _tup new)))]))])
         (P tup '()))))

   (check-equal? (scramble '(1 1 1 3 4 2 1 1 9 2))
                 '(1 1 1 1 1 4 1 1 1 9))

   ))
