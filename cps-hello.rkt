#lang racket

(require racket/trace)

; Simple continuation
(let ([val (let/cc the-continuation
                (display "This will be executed\n")
                (the-continuation 5)
                (display "This will NOT be executed\n"))])
  (display val))

; Returned continuation
(let ([val (let/cc the-continuation the-continuation)])
  (if (procedure? val)
      (begin
        (display "First time through\n")
        (display "val is a continuation object\n")
        (val 5))
      (begin
        (display "Second time through\n")
        (display "val is ")
        (display val)
        (newline))))


