#lang racket
(define assq-sf
    (lambda (a l sk fk)
        (cond
            ((null? l) (fk a))
            ((eq? a (car (car l))) (sk (car l)))
            (else (assq-sf a (cdr l) sk fk))
        )
    )
)

(define fk
    (lambda (name)
        (cons name (quote (not-in-list)))
    )
)

(define sk
    (lambda (p)
        (cons (first p) (cons (+ 1 (second p)) '()))
    )
)

(define a 'apple)
(define b1 '())
(define b2 '((apple 1) (plum 2)))
(define b3 '((peach 3)))

(assq-sf a b1 sk fk)
(assq-sf a b2 sk fk)
(assq-sf a b3 sk fk)