#lang racket
(define atom?
    (lambda (x)
        (and (not (pair? x)) (not (null? x)))
    )
)

(define lexp?
    (lambda (l)
        (cond 
            ((null? l) #t)
            ((atom? l) #t)
            ((or (equal? (car l) 'AND) (equal? (car l) 'OR)) (and (lexp? (car (cdr l))) (lexp? (car (cdr (cdr l))))))
            (else #f)
        )
    )
)
(lexp? '(AND (OR x y) (OR b c)))
(lexp? '(+ 2 3))
