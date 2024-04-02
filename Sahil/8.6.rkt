#lang racket
(define Rapply
    (lambda (r x)
        (cond
            ((null? r) '())
            ((eq? (car (car r)) x) (cons (car (cdr (car r))) (Rapply (cdr r) x)))
            (else (Rapply (cdr r) x)) 
        )
    )
)

(define f1 '((a 1) (b 2) (c 2) (d 1)))
(define r1 '((a b) (a a) (b b)))
(define f2 '())
(Rapply f1 'a)
(Rapply r1 'a)
(Rapply f2 'a)