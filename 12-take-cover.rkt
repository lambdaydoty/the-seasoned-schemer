#lang racket
(require "utils.rkt")
(provide (all-defined-out))
(require racket/trace)

;;
;; Note: Local Bindings:
;;       1) let
;;       2) let*
;;       3) letrec
;;

#| (define (multirember a lat) |#
#|   ((Y (λ (self) |#
#|         (λ (lat) |#
#|           (match lat |#
#|             [`(,(== a) ,rest ...) (self rest)] |#
#|             [`(,x ,rest ...) (list* x (self rest))] |#
#|             [else '()])))) |#
#|    lat)) |#

(define (multirember a a-lat)
  (letrec ([_self_ (λ (lat)
                     (match lat
                       [`(,(== a) ,rest ...) (_self_ rest)]
                       [`(,x ,rest ...) (list* x (_self_ rest))]
                       [else '()]))])
    (_self_ a-lat)))
