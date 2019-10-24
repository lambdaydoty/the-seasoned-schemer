#lang racket
(require "utils.rkt")
(provide (all-defined-out))
(require racket/trace)
(require rackunit)

(define kons mcons)
(define kdr mcdr)
(define kar mcar)
(define set-kdr! set-mcdr!)

(define kounter (λ () #f))
(define set-kounter (λ () #f))
(define konsC
  (let ([N 0])
    (set! kounter (λ () N)) ; getter
    (set! set-kounter (λ (x) (set! N x))) ; setter
    (λ (x y)
      (set! N (add1 N))
      (kons x y))))

(define lots
  (λ (m)
    (cond [(zero? m) '()]
          [else (kons 'egg (lots (sub1 m)))])))

(define lenkth
  (λ (l)
    (cond [(null? l) 0]
          [else (add1 (lenkth (kdr l)))])))

(define add-at-end
  (λ (l)
    (match l
      [(mcons x '()) (konsC x (kons 'egg '()))]
      [(mcons x y) (konsC x (add-at-end y))])))

(check-equal? (add-at-end (lots 3)) (kons 'egg
                                          (kons 'egg
                                                (kons 'egg
                                                      (kons 'egg '())))))
(check-equal? (kounter) 3)

(define add-at-end-too
  (λ (l)
    (letrec ([A (λ (ls)
                  (match ls
                    [(mcons x '()) (set-kdr! ls (kons 'egg '()))]
                    [else (A (kdr ls))]))])
      (A l)
      l)))

(set-kounter 0)
(check-equal? (kounter) 0)

(check-equal? (add-at-end-too (lots 3)) (kons 'egg
                                              (kons 'egg
                                                    (kons 'egg
                                                          (kons 'egg '())))))
(check-equal? (kounter) 0)

;

(set! kons
  (λ (kar kdr)
    (λ (selector)
      (selector kar kdr))))

(set! kar
  (λ (pair)
    (pair (λ (x y) x))))

(set! kar
  (λ (pair)
    (pair (λ (x y) y))))

;

((λ ()

   (define bons
     (λ (kar)
       (let ([kdr '()])
         (λ (selector)
           (selector (λ (x) (set! kdr x))
                     kar
                     kdr)))))

   (define kar
     (λ (pair)
       (pair (λ (s x y) x))))

   (define kdr
     (λ (pair)
       (pair (λ (s x y) y))))

   (define set-kdr!
     (λ (pair z)
       ((pair (λ (s x y) s)) z)))

   (check-equal? (kar (bons 'egg)) 'egg)
   (check-equal? (kdr (bons 'egg)) '())

   (define kons
     (λ (x y)
       (let ([pair (bons x)])
         (set-kdr! pair y)
         pair)))

   (define kounter (λ () #f))

   (define set-kounter (λ () #f))

   (define konsC
     (let ([N 0])
       (set! kounter (λ () N)) ; getter
       (set! set-kounter (λ (x) (set! N x))) ; setter
       (λ (x y)
         (set! N (add1 N))
         (kons x y))))

   (define lots
     (λ (m)
       (cond [(zero? m) '()]
             [else (konsC 'egg (lots (sub1 m)))])))

   (define add-at-end
     (λ (l)
       (cond [(null? (kdr l)) (konsC (kar l)
                                     (konsC 'egg '()))]
             [else (konsC (kar l)
                          (add-at-end (kdr l)))])))

   (define add-at-end-too
     (λ (l)
       (letrec ([A (λ (ls)
                     (cond
                       [(null? (kdr ls)) (set-kdr! ls (konsC 'egg '()))]
                       [else (A (kdr ls))]))])
         (A l)
         l)))

   (define dozen (lots 12))

   (check-equal? (kounter) 12)

   (define bakers-dozen (add-at-end dozen))

   (check-equal? (kounter) 25)

   (define bakers-dozen-too (add-at-end-too dozen))

   (check-equal? (kounter) 26)

   (define bakers-dozen-again (add-at-end dozen))

   (check-equal? (kounter) 40)

   (define eklist?
     (λ (ls1 ls2)
       (cond [(null? ls1) (null? ls2)]
             [(null? ls2) #f]
             [else (and (eq? (kar ls1) (kar ls2))
                        (eklist? (kdr ls1) (kdr ls2)))])))

   (check-equal? (eklist? bakers-dozen bakers-dozen-too) #t)

   (define same?
     (λ (c1 c2)
       (let ([t1 (kdr c1)]
             [t2 (kdr c2)])
         (set-kdr! c1 1)
         (set-kdr! c2 2)
         (let ([v (= (kdr c1)
                     (kdr c2))])
           (set-kdr! c1 t1)
           (set-kdr! c2 t2)
           v))))

   (check-equal? (same? dozen bakers-dozen-too) #t) ; ! errata !

   (check-equal? (same? (kons 'egg '())
                        (kons 'egg '())) #f)

   (define last-kons
     (λ (ls)
       (cond [(null? (kdr ls)) ls]
             [else (last-kons (kdr ls))])))

   (define long (lots 12))

   (set-kdr! (last-kons long) long)
   #| (set-kdr! (last-kons long) (kdr (kdr long))) |#

   (define finite-lenkth
     (λ (pair)
       (let/cc infinite
              (letrec ([C (λ (p q)
                            (cond [(null? q) 0]
                                  [(null? (kdr q)) 1]
                                  [(same? p q) (infinite #f)]
                                  [else (+ (C (sl p)
                                              (qk q))
                                           2)]))]
                       [qk (λ (x) (kdr (kdr x)))]
                       [sl (λ (x) (kdr x))])
                (cond [(null? pair) 0]
                      [else (add1 (C pair (kdr pair)))])))))

   (check-equal? (finite-lenkth (lots 12)) 12)
   (check-equal? (finite-lenkth long) #f)

   ))
