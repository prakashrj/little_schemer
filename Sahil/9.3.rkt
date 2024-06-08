#lang racket

(define Y2
    (lambda (M)
        (
            (lambda (future )
                (M (lambda (arg1 arg2 )
                    ((future future ) arg1 arg2 )
                    )
                )
            )
            (lambda (future )
                (M (lambda (arg1 arg2 )
                    ((future future ) arg1 arg2 )
                    )
                )
            )
        )
    )
)  

(define rempick
    (Y2 
        (lambda (s)
            (lambda (n l)
                (cond
                    ((zero? (- n 1)) (cdr l))
                    (else (cons (car l) (s (- n 1) (cdr l))))
                )
            )
        )
    )
)

(define pick
    (Y2
        (lambda (s)
            (lambda (n l)
                (cond
                    ((zero? (- n 1)) (car l))
                    (else (s (- n 1) (cdr l)))
                )
            )
        )
    )
)

(rempick 3 '(hotdogs with hot mustard))
(pick 3 '(hotdogs with hot mustard))