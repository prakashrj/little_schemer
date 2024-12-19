#lang racket

(define-syntax syntax-reverse
  (syntax-rules ()
    ((_ lst) (syntax-reverse lst ()))
    ((_ () r) r)
    ((_ (a . r) d) (syntax-reverse r (a . d)))))

(syntax-reverse (7 4 cons))
(syntax-reverse (4 7 cons))
