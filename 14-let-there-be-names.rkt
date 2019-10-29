#lang racket
(require "utils.rkt")
(require rackunit)
(require racket/trace)

;
; # Note
;
; ## Local Bindings and Continuations
;
; 1. `let`
; 2. `let*`
; 3. `letrec`
; 4. `let/cc`
;

((λ ()

   (define leftmost
     (λ (l)
       (match l
         [`(,(? atom? x) ,_l ...) x]
         [`(,s ,_l ...) (leftmost s)])))

   (check-equal? (leftmost '(((a) b) (c d))) 'a)
   (check-equal? (leftmost '(((a) ()) () (e))) 'a)
   #| (check-equal? (leftmost '(((() a) ()))) 'a) |#

   ))

((λ () ; respect the empty list input

   (define leftmost
     (λ (l)
       (match l
         [`() '()]
         [`(,(? atom? x) ,_l ...) x]
         [`(,s ,_l ...) (if (atom? (leftmost s))
                            (leftmost s)
                            (leftmost _l))])))

   (check-equal? (leftmost '(((a) b) (c d))) 'a)
   (check-equal? (leftmost '(((a) ()) () (e))) 'a)
   (check-equal? (leftmost '(((() a) ()))) 'a)

   ))

((λ () ; introduce (let ...)

   (define leftmost
     (λ (l)
       (match l
         [`() '()]
         [`(,(? atom? x) ,_l ...) x]
         [`(,s ,_l ...) (let ([a (leftmost s)])
                          (if (atom? a)
                              a
                              (leftmost _l)))])))

   (check-equal? (leftmost '(((a) b) (c d))) 'a)
   (check-equal? (leftmost '(((a) ()) () (e))) 'a)
   (check-equal? (leftmost '(((() a) ()))) 'a)

   ))

((λ ()

   (define rember1*
     (λ (a l)
       (match l
         [`() '()]
         [`(,(? atom? x) ,_l ...) (if (eq? a x)
                                      _l
                                      (list* x (rember1* a _l)))]
         [`(,s ,_l ...) (if (equal? (rember1* a s) s)
                            (list* s (rember1* a _l))
                            (list* (rember1* a s) _l))])))

   (check-equal? (rember1* 'salad '((Swedish rye) (French (mustard salad turkey) salad)))
                 '((Swedish rye) (French (mustard turkey) salad)))
   (check-equal? (rember1* 'meat '((pasta meat) pasta (noodles meat sauce) meat tomatoes))
                 '((pasta) pasta (noodles meat sauce) meat tomatoes))

   ))

((λ () ; rember1* with (let ...) (letrec ...)

   (define rember1*
     (λ (a l)
       (letrec ([R (λ (l)
                     (match l
                       [`() '()]
                       [`(,(? atom? x) ,_l ...) (if (eq? a x)
                                                    _l
                                                    (list* x (R _l)))]
                       [`(,s ,_l ...) (let ([av (R s)])
                                        (if (equal? av s)
                                            (list* s (R _l))
                                            (list* av _l)))]))])
         (R l))))

   (check-equal? (rember1* 'salad '((Swedish rye) (French (mustard salad turkey) salad)))
                 '((Swedish rye) (French (mustard turkey) salad)))
   (check-equal? (rember1* 'meat '((pasta meat) pasta (noodles meat sauce) meat tomatoes))
                 '((pasta) pasta (noodles meat sauce) meat tomatoes))

   ))

((λ ()

   (define depth*
     (λ (l)
       (match l
         [`() 1]
         [`(,(? atom? x) ,_l ...) (depth* _l)]
         [`(,s ,_l ...) (max (add1 (depth* s))
                             (depth* _l))])))

   (check-equal? (depth* '((pickled) peppers (peppers pickled))) 2)
   (check-equal? (depth* '(margarine ((bitter butter) (makes) (batter (bitter))) butter)) 4)
   (check-equal? (depth* '(c (b (a b) a) a)) 3)

   ))

; let/cc

((λ ()

   (define leftmost
     (λ (l)                                                  ; The function ...
       (let/cc skip                                          ; sets up a North Pole in `skip`
         (letrec ([LM (λ (l)                                 ; The function LM ...
                        (match l                             ; looks at every atom in `l` ...
                          [`() '()]                          ; from left to right until ...
                          [`(,(? atom? x) ,_l ...) (skip x)] ; it finds an atom and then uses `skip` to return this atom abruptly
                          [`(,s ,_l ...) (let ()
                                          (LM s)
                                          (LM _l))]))])
           #| (trace LM) |#
           (LM l)))))
   #| (trace leftmost) |#

   (check-equal? (leftmost '(((a)) b (c))) 'a)
   (check-equal? (leftmost '(((a) b) (c d))) 'a)
   (check-equal? (leftmost '(((a) ()) () (e))) 'a)
   (check-equal? (leftmost '(((() a) ()))) 'a)

   ))

((λ () ; rember1* with let/cc

   (define (rember1* a l)
     (letrec ([RM (λ (l oh)
                    (match l
                      [`() (oh 'NO)]
                      [`(,(? atom? x) ,_l ...) (if (eq? a x)
                                                   _l
                                                   (list* x (RM _l oh)))]
                      [`(,s ,_l ...) (let ([new-s (let/cc oh (RM s oh))])
                                       (if (atom? new-s) ; 'NO
                                           (list* s (RM _l oh))
                                           (list* new-s _l)))]))])
       #| (trace RM) |#
       (let ([new-l (let/cc oh (RM l oh))])
         (if (atom? new-l)
             l ; not found
             new-l))))

   #| (trace rember1*) |#

   (check-equal? (rember1* 'noodles '((food) more (food)))
                 '((food) more (food)))
   (check-equal? (rember1* 'salad '((Swedish rye) (French (mustard salad turkey) salad)))
                 '((Swedish rye) (French (mustard turkey) salad)))
   (check-equal? (rember1* 'meat '((pasta meat) pasta (noodles meat sauce) meat tomatoes))
                 '((pasta) pasta (noodles meat sauce) meat tomatoes))

   ))

((λ () ; rember1* with (try k α β)

   (define (rember1* a l)
     (letrec ([RM (λ (l oh)
                    (match l
                      [`() (oh 'NO)]
                      [`(,(? atom? x) ,_l ...) (if (eq? a x)
                                                   _l
                                                   (list* x (RM _l oh)))]
                      [`(,s ,_l ...) (try oh2
                                          (list* (RM s oh2)_l)
                                          (list* s (RM _l oh)))]))])
       (try oh
            (RM l oh)
            l)))

   (check-equal? (rember1* 'noodles '((food) more (food)))
                 '((food) more (food)))
   (check-equal? (rember1* 'salad '((Swedish rye) (French (mustard salad turkey) salad)))
                 '((Swedish rye) (French (mustard turkey) salad)))
   (check-equal? (rember1* 'meat '((pasta meat) pasta (noodles meat sauce) meat tomatoes))
                 '((pasta) pasta (noodles meat sauce) meat tomatoes))

   ))
