#lang racket
(require "utils.rkt")
(require rackunit "11-welcome-back-to-the-shadow.rkt")

(check-equal? (member? 'sardines
                       '(Italian sardines spaghetti parsley))
              #t)

(check-equal? (two-in-a-row? '(Italian sardines spaghetti parsley)) #f)
(check-equal? (two-in-a-row? '(Italian sardines sardines spaghetti parsley)) #t)
(check-equal? (two-in-a-row? '(Italian sardines more spaghetti parsley)) #f)
(check-equal? (two-in-a-row? '(b d e i i a g)) #t)

(check-equal? (sum-of-prefixes '(2 1 9 17 0)) '(2 3 12 29 29))

(check-equal? (scramble '(1 1 1 3 4 2 1 1 9 2))
              '(1 1 1 1 1 4 1 1 1 9))
