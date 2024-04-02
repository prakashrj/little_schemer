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

(define Fcomp
    (lambda (f g)
        (cond
            ((null? g) '())
            ((equal? (evalf f (car (cdr (car g)))) 'not-defined) (Fcomp f (cdr g)))
            (else 
                (define a (car (car g)))
                (define e (evalf f (car (cdr (car g)))))
                (cons (cons a (cons e '())) (Fcomp f (cdr g)))
            )
        )
    )
)

(define f4 '((1 $) (3 *)))
(define f1 '((a 1) (b 2) (c 2) (d 1)))
(define f3 '((a 2) (b 1)))
(Fcomp f1 f4)
(Fcomp f3 f4)
(Fcomp f4 f1)
(Fcomp f4 f3)