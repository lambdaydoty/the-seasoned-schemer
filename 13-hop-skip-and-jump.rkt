#lang racket
(require "utils.rkt")
(require rackunit)
(require racket/trace)

;
; NOTE: call/cc, let/cc
;

(define intersect
  (λ (set1 set2)
    (match set1
      [`() '()]
      [`(,x ,_set ...) (if (member? x set2)
                           (list* x (intersect _set set2))
                           (intersect _set set2))])))

(check-equal? (intersect '(tomatoes and macarnoi)
                         '(macarnoi and cheese))
              '(and macarnoi))

((λ () ; intersectall

   (define intersectall
     (λ (lset)
       (match lset
         [`(,x) x] ; !assume nonempty
         [`(,x ,_lset ...) (intersect x
                                      (intersectall _lset))])))

   (check-equal? (intersectall '((3 mangos and)
                                 (3 kiwis and)
                                 (3 hamburgers)))
                 '(3))
   (check-equal? (intersectall '((1 2 3)
                                 (2 3 4)
                                 (3 4 5)
                                 (4 5 6)))
                 '())

   ))

((λ () ; intersectall guarding empty list

   (define intersectall
     (λ (lset)
       (letrec ([IA (λ (lset)
                      (match lset
                        [`(,x) x]
                        [`(,x ,_lset ...) (intersect x (IA _lset))]))])
         (match lset
           [`() '()]
           [else (IA lset)]))))

   (check-equal? (intersectall '((3 mangos and)
                                 (3 kiwis and)
                                 (3 hamburgers)))
                 '(3))
   (check-equal? (intersectall '((1 2 3)
                                 (2 3 4)
                                 (3 4 5)
                                 (4 5 6)))
                 '())
   (check-equal? (intersectall '()) '())

   ))

((λ () ; intersectall with hop

   (define intersectall
     (λ (lset)
       (let/cc hop
         (letrec ([IA (λ (lset)
                        (match lset
                          [`(,(? null? x) ,_ ...) (hop "empty")]
                          [`(,x) x]
                          [`(,x ,_lset ...) (intersect x (IA _lset))]))])
           #| (trace IA) |#
           (match lset
             [`() '()]
             [else (IA lset)])))))

   (check-equal? (intersectall '((3 mangos and)
                                 (3 kiwis and)
                                 (3 hamburgers)))
                 '(3))
   (check-equal? (intersectall '((1 2 3)
                                 (2 3 4)
                                 (3 4 5)
                                 (4 5 6)))
                 '())
   (check-equal? (intersectall '()) '())
   (check-equal? (intersectall '((3 mangos and)
                                 ()
                                 (3 diet hamburgers)))
                 "empty")

   ))

;
; The 14th Commandment
;
; Use (let/cc ...) to return values abruptly and promptly.
;

((λ () ; intersectall with hop in intersect

   (define intersectall
     (λ (lset)
       (let/cc hop
         (letrec ([A (λ (lset)
                        (match lset
                          [`(() ,_ ...) (hop "empty!")]
                          [`(,x) x]
                          [`(,x ,_lset ...) (I x (A _lset))]))]
                  [I (λ (s1 s2)
                       (letrec ([J (λ (s1)
                                     (match s1
                                       [`() '()]
                                       [`(,x ,_s1 ...) (if (member? x s2)
                                                           (list* x (J _s1))
                                                            (J _s1))]))])
                         (match s2
                           [`() (hop "empty!!")]
                           [else (J s1)])))])
           #| (trace A) |#
           #| (trace I) |#
           (match lset
             [`() '()]
             [else (A lset)])))))

   (check-equal? (intersectall '((3 mangos and)
                                 (3 kiwis and)
                                 (3 hamburgers)))
                 '(3))
   (check-equal? (intersectall '((1 2 3)
                                 (2 3 4)
                                 (3 4 5)
                                 (4 5 6)))
                 '())
   (check-equal? (intersectall '()) '())
   (check-equal? (intersectall '((3 mangos and)
                                 ()
                                 (3 diet hamburgers)))
                 "empty!")
   (check-equal? (intersectall '((3 steaks and)
                                 (no food and)
                                 (three baked potatoes)
                                 (3 diet hamburgers)))
                 "empty!!")
   ))

;

(define rember
  (λ (a lat)
    (letrec ([R (λ (lat)
                  (match lat
                    [`() '()]
                    [`(,x ,_lat ...) (if (eq? a x)
                                         _lat
                                         (list* x (R _lat)))]))])
      (R lat))))

(define rember-beyond-first
  (λ (a lat)
    (letrec ([R (λ (lat)
                  (match lat
                    [`() '()]
                    [`(,x ,_lat ...) (if (eq? a x)
                                         '()
                                         (list* x (R _lat)))]))])
      (R lat))))

(define rember-upto-last
  (λ (a lat)
    (let/cc skip
      (letrec ([R (λ (lat)
                  (match lat
                    [`() '()]
                    [`(,x ,_lat ...) (if (eq? a x)
                                         (skip (R _lat)) ; <- :)
                                         (list* x (R _lat)))]))])
        #| (trace R) |#
        (R lat)))))

(let ([lat1 '(noodles spaghetti spatzle bean-thread roots potatoes yam others rice)]
      [lat2 '(cookies chocolate mints caramel delight ginger snaps
                      desserts chocolate mousse vanilla ice cream
                      German chocolate cake more desserts gingerbreadman
                      chocolate chip brownies)]
      [lat3 '(cookies chocolate mints caramel delight ginger snaps
                      desserts chocolate mousse vanilla ice cream
                      German chocolate cake more cookies gingerbreadman
                      chocolate chip brownies)])
  (check-equal? (rember-beyond-first 'roots lat1) '(noodles spaghetti spatzle bean-thread))
  (check-equal? (rember-beyond-first 'others lat1) '(noodles spaghetti spatzle bean-thread roots potatoes yam))
  (check-equal? (rember-beyond-first 'sweetthing lat1) lat1)
  (check-equal? (rember-beyond-first 'desserts lat2) '(cookies chocolate mints caramel delight ginger snaps))
  (check-equal? (rember-upto-last 'roots lat1) '(potatoes yam others rice))
  (check-equal? (rember-upto-last 'sweetthing lat1) lat1)
  (check-equal? (rember-upto-last 'cookies lat3) '(gingerbreadman chocolate chip brownies))
  )
