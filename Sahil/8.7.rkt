#lang racket
(define Rin
    (lambda (x s)
        (cond
            ((null? s) '())
            (else (cons (cons x (cons (car s) '())) (Rin x (cdr s))))
        )
    )
)

(define d1 '(a b))
(define d2 '(c d))
(define f2 '())
(Rin 'a d1)
(Rin 'a d2)
(Rin 'a f2)