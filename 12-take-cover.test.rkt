#lang racket
(require "utils.rkt")
(require rackunit "12-take-cover.rkt")

(check-equal? (multirember 'i '(b d e i i a g))
              '(b d e a g))
