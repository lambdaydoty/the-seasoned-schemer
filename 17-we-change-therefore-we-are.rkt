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
       (letrec ([D (λ (m)
                  (if (zero? m)
                      'pizza
                      (list (deepM (sub1 m)))))])
         (λ (n)
           (let ([exists (find n Ns Rs)])
             (if (atom? exists) ; #f
                 (let ([result (D n)])
                   (set! Rs (cons result Rs))
                   (set! Ns (cons n Ns))
                   result)
                 exists))))))

   (check-equal? (deepM 0) 'pizza)
   (check-equal? (deepM 5) '(((((pizza))))))

   ))

((λ () ; remove letrec

   (define deepM
     (let ([Rs '()]
           [Ns '()]
           [D (λ (m)
                (if (zero? m)
                    'pizza
                    (list (deepM (sub1 m)))))])
       (λ (n)
         (let ([exists (find n Ns Rs)])
           (if (atom? exists) ; #f
               (let ([result (D n)])
                 (set! Rs (cons result Rs))
                 (set! Ns (cons n Ns))
                 result)
               exists)))))

   (check-equal? (deepM 0) 'pizza)
   (check-equal? (deepM 5) '(((((pizza))))))

   ))

((λ () ; remove D

   (define deepM
     (let ([Rs '()]
           [Ns '()])
       (λ (n)
         (let ([exists (find n Ns Rs)])
           (if (atom? exists) ; #f
               (let ([result (if (zero? n)
                                 'pizza
                                 (list (deepM (sub1 n))))])
                 (set! Rs (cons result Rs))
                 (set! Ns (cons n Ns))
                 result)
               exists)))))

   (check-equal? (deepM 0) 'pizza)
   (check-equal? (deepM 5) '(((((pizza))))))

   ))

((λ () ; count cons

   (define counter #f)

   (define set-counter #f)

   (define consC
     (let ([N 0])
       (set! counter (λ () N))               ; getter
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

   ; to determine 500,500

   (define supercounter
     (λ (f)
       (letrec
         ([S (λ (n)
               (if (zero? n)
                   (f n)
                   (let ()
                     (f n)
                     (S (sub1 n)))))])
         (S 1000)
         (counter))))

   (check-equal? (supercounter deep) 500512)

   (set-counter 0)

   (check-equal? (supercounter deep) 500500) ; 1000 + 999 + 998 + ... + 2 + 1 = 500,500

   (define deepM ; inject consC into deepM
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

; rember1*

((λ ()

   (define rember1*
     (λ (a l)
       (letrec
         ([R (λ (l oh)
               (match l
                 [`() (oh 'no)]
                 [`(,(? atom? x) ,_l ...) (if (eq? (x a))
                                             _l
                                             (cons x (R _l oh)))]
                 [`(,s ,_l ...) (let ([new-s (let/cc oh (R s oh))])
                                 (if (atom? new-s)
                                     (cons s (R _l oh))
                                     (cons new-s _l)))]))])
         (let ([new-l (let/cc oh (R l oh))])
           (if (atom? new-l)
               l
               new-l)))))

   (define counter #f)
   (define set-counter #f)
   (define consC
     (let ([N 0])
       (set! counter (λ () N))              ; getter
       (set! set-counter (λ (x) (set! N x))) ; setter
       (λ (x y)
         (set! N (add1 N))
         (cons x y))))

   (define rember1*C
     (λ (a l)
       (letrec ([R (λ (l oh)
                     (match l
                       [`() (oh 'no)]
                       [`(,(? atom? x) ,_l ...) (if (eq? x a)
                                                   _l
                                                   (consC x (R _l oh)))]
                       [`(,s ,_l ...) (let ([new-x (let/cc oh (R s oh))])
                                       (if (atom? new-x)
                                           (consC s (R _l oh))
                                           (consC new-x _l)))]))])
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
       (letrec
         ([R (λ (l)
               (match l
                 [`() '()]
                 [`(,(? atom? x) ,_l ...) (if (eq? x a)
                                             _l
                                             (consC x (R _l)))]
                 [`(,s ,_l ...) (let ([av (R s)])
                                 (if (equal? s av)
                                     (consC s (R _l))
                                     (consC av _l)))]))])
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
