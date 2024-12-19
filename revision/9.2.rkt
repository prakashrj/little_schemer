#lang racket

(define assq-sf
  (lambda (a l sk fk)
    (cond
      ((null? l)(fk a))
      ((equal? (car (car l)) a) (sk (car l)))
      (else (assq-sf a (cdr l) sk fk)))))



(define sk 
 (lambda (p)
   (cons (first p) (+ 1 (second p)))))

(define fk
   (lambda (name)
     (cons name '(not-in-list))))

(define b1 '())
(define b2 '((apple 1) (plum 2)))
(define b3 '((peach 3)))
(define a 'apple)

(assq-sf a b1 sk fk)
(assq-sf a b2 sk fk)
(assq-sf a b3 sk fk)

