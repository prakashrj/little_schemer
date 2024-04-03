#lang racket
(define search
    (lambda (a l)
        (cond
            ((null? l) #f)
            ((eq? (car l) a) #t)
            (else (search a (cdr l)))
        )
    )
)

(define loosen
    (lambda (r)
        (cond
            ((null? r) '())
            (else (cons (car (car r)) (cons (car (cdr (car r))) (loosen (cdr r)))))
        )
    )
)

(define removedup
    (lambda (r)
        (cond
            ((null? r) '())
            ((search (car r) (removedup (cdr r))) (removedup (cdr r)))
            (else (cons (car r) (removedup (cdr r))))
        )
    )
)

(define domset
    (lambda (r)
        (removedup (loosen r))
    )
)

(define idrel
    (lambda (s)
        (cond 
            ((null? s) '())
            (else (cons (cons (car s) (cons (car s) '())) (idrel (cdr s))))
        )
    )
)


(define r1 '((a b) (a a) (b b)))
(define r2 '((c c)))
(define r3 '((a c) (b c)))
(domset r1)
(domset r2)
(domset r3)
(idrel '(a 2 3))