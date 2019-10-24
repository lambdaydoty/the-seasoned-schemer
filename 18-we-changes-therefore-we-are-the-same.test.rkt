#lang racket
(require "utils.rkt")
(require rackunit "18-we-changes-therefore-we-are-the-same.rkt")

(check-equal? (lots 3) '(egg egg egg))
(check-equal? (lots 5) '(egg egg egg egg egg))
(check-equal? (lots 12) '(egg egg egg egg egg egg egg egg egg egg egg egg))

(check-equal? (lenkth (lots 3)) 3)
(check-equal? (lenkth (lots 5)) 5)
(check-equal? (lenkth (lots 15)) 15)



