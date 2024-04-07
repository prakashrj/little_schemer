#lang racket
(define search
    (lambda (a l)
        (cond
            ((null? l) #f)
            ((equal? (car l) a) #t)
            (else (search a (cdr l)))
        )
    )
)

(define revrel
    (lambda (r)
        (cond
            ((null? r) '())
            (else (cons (cons (car (cdr (car r))) (cons (car (car r)) '())) (revrel (cdr r))))
        )
    )
)

(define eqset?
    (lambda (s r)
        (cond
            ((null? s) #t)
            ((search (car s) r) (eqset? (cdr s) r))
            (else #f)
        )
    )
)

(define symmetric?
    (lambda (r)
        (eqset? (revrel r) r)
    )
)

(define r1 '((a b) (a a) (b b)))
(define r2 '((c c)))
(define r3 '((a c) (b c)))
(define f2 '())
(symmetric? r1)
(symmetric? r2)
(symmetric? r3)
(symmetric? f2)