#lang racket
(require "utils.rkt")
(require rackunit "14-let-there-be-names.rkt")

(check-equal? (leftmost '(((a) b) (c d))) 'a)
(check-equal? (leftmost '(((a) ()) () (e))) 'a)
(check-equal? (leftmost '(((() a) ()))) 'a)

(check-equal? (rember1* 'salad '((Swedish rye)
                                 (French (mustard salad turkey)
                                         salad)))
              '((Swedish rye)
                (French (mustard turkey)
                salad)))
(check-equal? (rember1* 'meat '((pasta meat)
                                pasta
                                (noodles meat sauce)
                                meat tomatoes))
              '((pasta)
                pasta
                (noodles meat sauce)
                meat tomatoes))

(check-equal? (depth* '((pickled) peppers (peppers pickled))) 2)
(check-equal? (depth* '(margarine ((bitter butter) (makes) (batter (bitter))) butter)) 4)
(check-equal? (depth* '(c (b (a b) a) a)) 3)
