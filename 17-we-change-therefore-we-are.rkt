#lang racket
(require "utils.rkt")
(provide (all-defined-out))
(require racket/trace)
(require rackunit)

(define find
  (λ (n Ns Rs)
    (letrec ([A (λ (ns rs)
                  (cond [(null? ns) #f]
                        [(= (car ns) n) (car rs)]
                        [else (A (cdr ns) (cdr rs))]))])
      (A Ns Rs))))

#| (check-equal? (sweet-tooth 'chocolate) '(chocolate cake)) |#

((λ ()
   (define deep
     (λ (m)
       (if (zero? m)
           'pizza
           (list (deep (sub1 m))))))
   (check-equal? (deep 5) '(((((pizza))))))
   ))

((λ ()

   (define deepM
     (let ([Rs '()]
           [Ns '()])
       (λ (n)
         (let ([exists (find n Ns Rs)])
           (if (atom? exists)
               (let ([result (if (zero? n)
                                 'pizza
                                 (list (deepM (sub1 n))))])
                 (set! Rs (cons result Rs))
                 (set! Ns (cons n Ns))
                 result)
               exists)))))

   #| (trace find) |#
   #| (trace deepM) |#

   (check-equal? (deepM 0) 'pizza)
   (check-equal? (deepM 5) '(((((pizza))))))

   ))

((λ () ; count cons

   (define counter (λ () #f))
   (define set-counter (λ () #f))
   (define consC
     (let ([N 0])
       (set! counter (λ () N)) ; getter
       (set! set-counter (λ (x) (set! N x))) ; setter
       (λ (x y)
         (set! N (add1 N))
         (cons x y))))

   (define deep
     (λ (m)
       (if (zero? m)
           'pizza
           (consC (deep (sub1 m))
                  '()))))

   (check-equal? (counter) 0)

   (check-equal? (deep 0) 'pizza)
   (check-equal? (counter) 0)

   (check-equal? (deep 5) '(((((pizza))))))
   (check-equal? (counter) 5)

   (check-equal? (deep 7) '(((((((pizza))))))))
   (check-equal? (counter) 12)

   (define supercounter
     (λ (f)
       (letrec ([S (λ (n)
                     (if (zero? n)
                         (f n)
                         (let ()
                           (f n)
                           (S (sub1 n)))))])
       (S 1000)
       (counter))))

   (check-equal? (supercounter deep) 500512)

   (set-counter 0)

   (check-equal? (supercounter deep) 500500)

   (define deepM
     (let ([Rs '()]
           [Ns '()])
       (λ (n)
         (let ([exists (find n Ns Rs)])
           (if (atom? exists)
               (let ([result (if (zero? n)
                                 'pizza
                                 (consC (deepM (sub1 n))
                                        '()))])
                 (set! Rs (cons result Rs))
                 (set! Ns (cons n Ns))
                 result)
               exists)))))

   (set-counter 0)

   (check-equal? (deepM 5) '(((((pizza))))))
   (check-equal? (counter) 5)

   (check-equal? (deepM 7) '(((((((pizza))))))))
   (check-equal? (counter) 7) ; the result has been memorized!

   (check-equal? (supercounter deepM) 1000)

   ))

((λ ()

   (define rember1*
     (λ (a l)
       (letrec ([R (λ (l oh)
                     (match l
                       [`() (oh 'no)]
                       [`(,(? atom? x) ,y ...) (if (eq? (x a))
                                                   y
                                                   (cons x (R y oh)))]
                       [`(,x ,y ...) (let ([new-x (let/cc oh (R x oh))])
                                       (if (atom? new-x)
                                           (cons x (R y oh))
                                           (cons new-x y)))]))])
         (let ([new-l (let/cc oh (R l oh))])
           (if (atom? new-l)
               l
               new-l)))))

   (define counter (λ () #f))
   (define set-counter (λ () #f))
   (define consC
     (let ([N 0])
       (set! counter (λ () N)) ; getter
       (set! set-counter (λ (x) (set! N x))) ; setter
       (λ (x y)
         (set! N (add1 N))
         (cons x y))))

   (define rember1*C
     (λ (a l)
       (letrec ([R (λ (l oh)
                     (match l
                       [`() (oh 'no)]
                       [`(,(? atom? x) ,y ...) (if (eq? x a)
                                                   y
                                                   (consC x (R y oh)))]
                       [`(,x ,y ...) (let ([new-x (let/cc oh (R x oh))])
                                       (if (atom? new-x)
                                           (consC x (R y oh))
                                           (consC new-x y)))]))])
         #| (trace R) |#
         (let ([new-l (let/cc oh (R l oh))])
           (if (atom? new-l)
               l
               new-l)))))

   (set-counter 0)

   (check-equal? (rember1*C 'noodles '((food) more (food))) '((food) more (food)))
   (check-equal? (counter) 0)

   (define rember1*C2
     (λ (a l)
       (letrec ([R (λ (l)
                     (match l
                       [`() '()]
                       [`(,(? atom? x) ,y ...) (if (eq? x a)
                                                   y
                                                   (consC x (R y)))]
                       [`(,x ,y ...) (let ([av (R x)])
                                       (if (equal? x av)
                                           (consC x (R y))
                                           (consC av y)))]))])
         (R l))))

   (set-counter 0)

   (let ([f 'food]
         [m 'more])
         (check-equal? (consC (consC f '())
                              (consC m
                                     (consC (consC f '())
                                            '())))
                       '((food) more (food))))

   (check-equal? (counter) 5)

   (set-counter 0)

   (check-equal? (rember1*C2 'noodles '((food) more (food))) '((food) more (food)))

   (check-equal? (counter) 5) ; rember1*C2 needs five consCs to rebuild '((food) more (food))

   ))
