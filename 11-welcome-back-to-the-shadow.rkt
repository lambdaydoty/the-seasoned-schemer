#lang racket
(require "utils.rkt")
(require rackunit)
(require racket/trace)

#| (provide (all-defined-out)) |#

;;
;; Note:
;;


(define member?
  (λ (a lat)
    (match lat
      [`() #f]
      [`(,x ,_lat ...) (if (eq? a x)
                        #t
                        (member? a _lat))])))

(check-equal? (member? 'sardines
                       '(Italian sardines spaghetti parsley))
              #t)

((λ () ; two-in-a-row?

   (define is-first?
     (λ (a lat)
       (match lat
         [`() #f]
         [`(,x ,_ ...) (eq? a x)])))

   (define two-in-a-row?
     (λ (lat)
       (match lat
         [`() #f]
         [`(,x ,_lat ...) (if (is-first? x _lat)
                              #t
                              (two-in-a-row? _lat))])))

   (check-equal? (two-in-a-row? '(Italian sardines spaghetti parsley)) #f)
   (check-equal? (two-in-a-row? '(Italian sardines sardines spaghetti parsley)) #t)
   (check-equal? (two-in-a-row? '(Italian sardines more spaghetti parsley)) #f)
   (check-equal? (two-in-a-row? '(b d e i i a g)) #t)

   ))

((λ () ; two-in-a-row? (revised)

   (define is-first?
     (λ (a lat)
       (match lat
         [`() #f]
         [`(,x ,_ ...) (if (eq? a x)
                           #t
                           (two-in-a-row? lat))])))

   (define two-in-a-row?
     (λ (lat)
       (match lat
         [`() #f]
         [`(,x ,_lat ...) (is-first? x _lat)])))

   (check-equal? (two-in-a-row? '(Italian sardines spaghetti parsley)) #f)
   (check-equal? (two-in-a-row? '(Italian sardines sardines spaghetti parsley)) #t)
   (check-equal? (two-in-a-row? '(Italian sardines more spaghetti parsley)) #f)
   (check-equal? (two-in-a-row? '(b d e i i a g)) #t)

   ))

((λ () ; two-in-a-row? (b-version)

   (define two-in-a-row-b?
     (λ (preceding lat)
       (match lat
         [`() #f]
         [`(,x ,_lat ...) (if (eq? preceding x)
                              #t
                              (two-in-a-row-b? x _lat))])))

   (define two-in-a-row?
     (λ (lat)
       (match lat
         [`() #f]
         [`(,x ,_lat ...) (two-in-a-row-b? x _lat)])))

   (check-equal? (two-in-a-row? '(Italian sardines spaghetti parsley)) #f)
   (check-equal? (two-in-a-row? '(Italian sardines sardines spaghetti parsley)) #t)
   (check-equal? (two-in-a-row? '(Italian sardines more spaghetti parsley)) #f)
   (check-equal? (two-in-a-row? '(b d e i i a g)) #t)

   ))

((λ () ; sum-of-prefixes

   (define sum-of-prefixes-b
     (λ (sonssf tup)
       (match tup
         [`() '()]
         [`(,x ,_tup ...) (let ([sum (+ sonssf x)])
                            (list* sum
                                  (sum-of-prefixes-b sum _tup)))])))

   (define sum-of-prefixes
     (λ (tup)
       (sum-of-prefixes-b 0 tup)))

   (check-equal? (sum-of-prefixes '(2 1 9 17 0)) '(2 3 12 29 29))
   (check-equal? (sum-of-prefixes '(1 1 1 1 1)) '(1 2 3 4 5))

   ))

;
; The 17th Commandment
;
; Use additional arguments when a function needs to know what other arguments to
; the function have been like so far.
;

; scramble

(define pick
  (λ (n lat)
    (cond [(one? n) (car lat)]
          [else (pick (sub1 n)
                      (cdr lat))])))

(define scramble-b
  (λ (tup rev-pre)
    (match tup
      [`() '()]
      [`(,x ,_tup ...) (let ([new-rev-pre (list* x rev-pre)])
                         (list* (pick x new-rev-pre)
                                (scramble-b _tup new-rev-pre)))])))

(define scramble
  (λ (tup)
    (scramble-b tup '())))

#| (trace scramble-b) |#

(check-equal? (scramble '(1 1 1 3 4 2 1 1 9 2))
              '(1 1 1 1 1 4 1 1 1 9))
