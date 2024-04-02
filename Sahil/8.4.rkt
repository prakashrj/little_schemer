#lang racket
(define evalf
    (lambda (f x)
        (cond
            ((null? f) 'not-defined)
            ((equal? (car (car f)) x) (car (cdr(car f))))
            (else (evalf (cdr f) x))
        )
    )
)

(define f1 '((a 1) (b 2) (c 2) (d 1)))
(evalf f1 'e)
(evalf f1 'c)