#lang racket
(require "utils.rkt")
(provide (all-defined-out))
(require racket/trace)

;;
;; Note:
;;

(define (member? a lat)
  (cond [(null? lat) #f]
        [else (if (eq? (car lat) a)
                  #t
                  (member? a (cdr lat)))]))

(define (pick n lat)
  (define (recur i lst)
    (cond [(zero? i) (car lst)]
          [else (recur (sub1 i) (cdr lst))]))
  (recur (sub1 n) lat))

(define (two-in-a-row? lat)
  (define (recur preceding lat)
    (cond [(null? lat) #f]
          [else (if (eq? (car lat) preceding)
                    #t
                    (recur (car lat)
                           (cdr lat)))]))
  #| (trace recur) |#
  (recur (car lat) (cdr lat)))

(define (sum-of-prefixes tup)
  (define (recur acc rest)
    (cond [(null? rest) '()]
          [else (cons (+ acc (car rest))
                      (recur (+ acc (car rest))
                             (cdr rest)))]))
  #| (trace recur) |#
  (recur 0 tup))

(define (scramble tup)
  (define (recur tup reverse-prefix)
    (cond [(null? tup) '()]
          [else (cons (pick (car tup)
                            (cons (car tup) reverse-prefix))
                      (recur (cdr tup)
                             (cons (car tup) reverse-prefix)))]))
  (trace recur)
  (recur tup '()))
