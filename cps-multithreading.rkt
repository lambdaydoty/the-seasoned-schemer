#lang racket
(require racket/trace)

;
; (thread-new some-proc)
;
; (thread-yield)
;
; (thread-end)
;

; mermaid sequence diagram:
;
;   https://mermaidjs.github.io/mermaid-live-editor/#/view/eyJjb2RlIjoic2VxdWVuY2VEaWFncmFtXG4gICAgbWFpbiAtPj4gdGhyZWFkMTogKHRocmVhZC1uZXcgdGhyZWFkMSlcbiAgICBOb3RlIGxlZnQgb2YgdGhyZWFkMTogXCJTdGFydCBmcm9tIDFcIlxuICAgIHRocmVhZDEgLS0-PiBtYWluOiAodGhyZWFkLXlpZWxkKVxuICAgIG1haW4gLT4-IHRocmVhZDI6ICh0aHJlYWQtbmV3IHRocmVhZDIpXG4gICAgTm90ZSBsZWZ0IG9mIHRocmVhZDI6IFwiU3RhcnQgZnJvbSAyXCJcbiAgICB0aHJlYWQyIC0tPj4gdGhyZWFkMTogKHRocmVhZC15aWVsZClcbiAgICBOb3RlIGxlZnQgb2YgdGhyZWFkMTogXCJFbmQgb2YgMVwiXG4gICAgdGhyZWFkMSAtLT4-IG1haW46ICh0aHJlYWQtZW5kKVxuICAgIG1haW4gLT4-IHRocmVhZDM6ICh0aHJlYWQtbmV3IHRocmVhZDMpXG4gICAgTm90ZSBsZWZ0IG9mIHRocmVhZDM6IFwiU3RhcnQgZnJvbSAzXCJcbiAgICB0aHJlYWQzIC0tPj4gdGhyZWFkMjogKHRocmVhZC15aWVsZClcbiAgICBOb3RlIGxlZnQgb2YgdGhyZWFkMjogXCJJbiB0aGUgbWlkd2F5IG9mIDJcIlxuICAgIHRocmVhZDIgLS0-PiBtYWluOiAodGhyZWFkLWVuZClcbiAgICBOb3RlIHJpZ2h0IG9mIG1haW46IFwiRW5kIG9mIG1haW5cIlxuICAgIG1haW4gLT4-IHRocmVhZDM6ICh0aHJlYWQtZW5kKVxuICAgIE5vdGUgbGVmdCBvZiB0aHJlYWQzOiBcIkVuZCBvZiAzXCJcbiAgICB0aHJlYWQzIC0tPj4gdGhyZWFkMjogKHRocmVhZC1lbmQpXG4gICAgTm90ZSBsZWZ0IG9mIHRocmVhZDI6IFwiRW5kIG9mIDJcIlxuICAgIHRocmVhZDIgLT4-IHRocmVhZDI6IChleGl0KSIsIm1lcm1haWQiOnsidGhlbWUiOiJkZWZhdWx0In19
;

(define thread-list '())

(define thread-yield
  (λ ()
    (match thread-list
      [`() #t]
      [`(,next ,rest ...) (let ([conti (let/cc c c)]) ; save the current thread state into a continuation
                            (if (procedure? conti)    ; from creating the continuation
                                (let ()
                                  (set! thread-list (append rest (list conti)))
                                  (next 'your-turn))
                                #t))])))             ; from calling the continuation

(define thread-end
  (λ ()
    (match thread-list
      [`() (exit)]
      [`(,next ,rest ...) (let ()
                           (set! thread-list rest)
                           (next 'your-turn))])))

(define thread-new
  (λ (thread-proc)
    (let ([conti (let/cc c c)])
      (if (procedure? conti)
          (let ()
            (set! thread-list (append thread-list (list conti)))
            (thread-proc)
            (thread-end))
          #t))))

; test

(define thread1
  (λ ()
    (display "Start from 1\n")
    (thread-yield)
    (display "End of 1\n")))

(define thread2
  (λ ()
    (display "Start from 2\n")
    (thread-yield)
    (display "In the midway of 2\n")
    (thread-yield)
    (display "End of 2\n")))

(define thread3
  (λ ()
    (display "Start from 3\n")
    (thread-yield)
    (display "End of 3\n")))

(thread-new thread1)
(thread-new thread2)
(thread-new thread3)

(display "End of main\n")

(thread-end)
