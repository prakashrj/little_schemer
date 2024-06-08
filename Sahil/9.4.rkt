#lang racket

(define Y
    (lambda (e)
        ((lambda (f) (f f))
            (lambda (f)
                (e (lambda (x) ((f f) x)))))))

(define rember-Y
    (lambda (a l)
        (   
            (Y (lambda (recfun)
                    (lambda (l)
                        (cond
                            ((null? l ) '())
                            ((eq? (car l) a) (cdr l))
                            (else (cons (car l) (recfun (cdr l))))
                        )
                    )
                )
            )
        l)
    )
)

(define insertR-Y
    (lambda (new old lat)
        (   
            (Y (lambda (recfun)
                    (lambda (l)
                        (cond
                            ((null? l ) '())
                            ((eq? (car l) old) (cons old (cons new (cdr l))))
                            (else (cons (car l) (recfun (cdr l))))
                        )
                    )
                )
            )
        lat)
    )
)

(define subst2-Y
    (lambda (new o1 o2 lat)
        (   
            (Y (lambda (recfun)
                    (lambda (l)
                        (cond
                            ((null? l ) '())
                            ((or (eq? (car l) o1) (eq? (car l) o2)) (cons new (cdr l)))
                            (else (cons (car l) (recfun (cdr l))))
                        )
                    )
                )
            )
        lat)
    )
)

(rember-Y 'cat '(mat cat bat cat))
(insertR-Y 'hat 'cat '(mat cat bat cat))
(subst2-Y 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping) )