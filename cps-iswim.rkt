#lang racket
(require racket/trace)

;
; https://www.cs.utah.edu/~mflatt/past-courses/cs6520/public_html/s02/cps.pdf
;

(displayln "1 Continuations and Accumumlators")

((λ ()

   (define sum
     (λ (n)
       (if (zero? n)
           0
           (+ n (sum (sub1 n))))))

   (trace sum)

   (sum 10)

   ))


((λ ()

   (define sum
     (λ (n acc)
       (if (zero? n)
           acc
           (sum (sub1 n) (+ acc n)))))

   (trace sum)

   (sum 10 0)

   ))

((λ ()

   (define make-list
     (λ (n)
       (if (zero? n)
           '()
           (cons n (make-list (sub1 n))))))

   (trace make-list)

   (make-list 10)

   ))

((λ ()

   (define make-list
     (λ (n acc)
       (if (zero? n)
           acc
           (make-list
             (sub1 n)
             (cons n acc)))))

   (trace make-list)

   (make-list 10 '())

   ))

(newline)
(displayln "2 Accumulating a Continuations")

((λ ()

   (define make-list
     (λ (n k)
       (if (zero? n)
           (k '())
           (make-list
             (sub1 n)
             (λ (l) (k (cons n l)))))))

   (trace make-list)

   (make-list 10 (λ (x) x))

   ))

(newline)
(displayln "3 Continuation-Passing Style")

((λ ()

   (define dont-make-list
     (λ (n k)
       (if (zero? n)
           'boom
           (dont-make-list
             (sub1 n)
             (λ (l) (k (cons n l)))))))

   (trace dont-make-list)

   (dont-make-list 10 (λ (x) x))

   ))

((λ ()

   (define map0
     (λ (fn l)
       (match l
         [`() '()]
         [`(,x ,_l ...) (list* (fn x) (map0 fn _l))])))

   (define map
     (λ (fn l k)
       (match l
         [`() (k '())]
         [`(,x ,_l ...) (fn x (λ (y)
                                (map fn _l (λ (z)
                                             (k (list* y z))))))])))

   (trace map)

   (map (λ (x f) (f (* x x)))
        '(1 2 3 4 5 6 7)
        (λ (x) x))

   ))
