#lang racket
(define atom?
    (lambda (x)
        (and (not (pair? x)) (not (null? x)))
    )
)

(define search
    (lambda (a l)
        (cond
            ((null? l) #f)
            ((eq? (car l) a) #t)
            (else (search a (cdr l)))
        )
    )
)

(define covered?
    (lambda (lexp los)
        (cond
            ((null? lexp) #t)
            ((atom? lexp) (search lexp los))
            ((or (equal? (car lexp) 'AND) (equal? (car lexp) 'OR)) (and (covered? (car (cdr lexp)) los) (covered? (car (cdr (cdr lexp))) los)))
        )
    )
)

(define lexp1 '(AND (OR x y) z))
(define los '(x y z))
(define los1 '(x y))
(define los2 '(x z))
(define los3 '(y z))
(define los4 '(a b y x z c))

(covered? lexp1 los)
(covered? lexp1 los1)
(covered? lexp1 los2)
(covered? lexp1 los4)
(covered? lexp1 los3)
