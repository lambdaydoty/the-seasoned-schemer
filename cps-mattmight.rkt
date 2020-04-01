#lang sicp

((lambda () ; Omniscience: SAT-solving

   (let ([a (amb '(1 2 3 4 5 6 7))]
         [b (amb '(1 2 3 4 5 6 7))]
         [c (amb '(1 2 3 4 5 6 7))])

     (require (= (* c c)
                (+ (* a a) (* b b))))

     (require (< b a))

     (displayln (list a b c)))

   ))
