#lang racket

;
; https://stackoverflow.com/questions/35064635/is-tail-recursion-merely-a-special-case-of-cps
;
; # Recursion: The calling of a function from within that same function
; # Tail Call: The last thing a function does before it returns is call another function
; # Tail Recursion: #1 and #2 combined
; # Direct Style: Sequential programming style characterized by functions, which return to their caller
; # Continuation Passing Style (CPS): Programming style characterized by functions with an additional continuation argument, which call their continuation instead of returning to their caller (continuations are simply functions in Javascript)
;
; How correlate these terms?
;
; # Direct Style and Continuation Passing Style are opposing concepts of control flow
; # Tail recursive calls are a specialization of tail calls
; # Recursion and tail recursion are techniques of Direct Style
; # Each (tail) recursive algorithm can be converted into its CPS form, because CPS has more expressive power than recursion
;

(define fn
  (λ (x)
    (cond
      [(zero? x) 17]
      [(= x 1) (f x)]
      [(= x 2) (+ 22 (f x))]
      [(= x 3) (p 22 (f x))]
      [(= x 4) (+ (f x) 33 (g y))]
      [else (h (f x)
               (- 44 y)
               (g y))])))

(define (f x) (+ x 1))
(define (g x) (* x 2))
(define (h x y z) (+ x y z))
(define (p x y) (* x y))
(define y 1)


(define id
  (λ (x)
    x))

(list
  (fn 0)
  (fn 1)
  (fn 2)
  (fn 3)
  (fn 4)
  (fn 5)
  )

; cps

(define fn/k
  (λ (x cont)
    (cond
      [(zero? x) (cont 17)]                      ; A) When the proc returns a const/var, return that const/var to the `cont`.
      [(= x 1) (f/k x cont)]                     ; B) When a proc call occurs in a tail position, call the proc w/ the same `cont`.
      [(= x 2) (f/k x (λ (v) (cont (+ 22 v))))]  ; C) When a proc call occurs in an operand position, eval the proc call in a new cont that gives a name to the result and continues w/ the computation
      [(= x 3) (f/k x (λ (v) (p/k 22 v cont)))]   ; C + B
      [(= x 4) (f/k x (λ (v1)
                      (g/k y (λ (v2)
                             (cont (+ v1 33 v2))))))] ; C + C
      [else (f/k x (λ (v1)
                   (g/k y (λ (v2)
                          (h/k v1
                             (- 44 y)
                             v2
                             cont)))))])))   ; C + C + B

(define (f/k x f) (f (+ x 1)))
(define (g/k x f) (f (* x 2)))
(define (h/k x y z f) (f (+ x y z)))
(define (p/k x y id) (* x y))

(list
  (fn/k 0 id)
  (fn/k 1 id)
  (fn/k 2 id)
  (fn/k 3 id)
  (fn/k 4 id)
  (fn/k 5 id)
  )
