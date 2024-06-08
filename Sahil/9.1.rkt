#lang racket
(define map
    (lambda (f)
       (lambda (l)
            (cond
                ((null? l) '())
                (else (cons (f (car l)) ((map f) (cdr l))))
            )
       )
    )
)

(define f
    (lambda(x)
        (+ x 5)
    )
)

(define firsts
    (lambda(x)
        (car x)
    )
)

(define seconds
    (lambda(x)
        (car (cdr x))
    )
)

((map f) '(5 15 25 35))
((map firsts)  '(((five plums) four) (eleven green oranges) ((no) more)))
((map seconds)  '(((five plums) four) (eleven green oranges) ((no) more)))