#lang racket

(define s 'sauce)

(define subst-sauce
  (lambda (a l)
    (cond
      ((null? l) '())
      ((eq? (car l) s) (cons a (cdr l)))
      (else (cons (car l) (subst-sauce a (cdr l)))))))

(define l '(texas hot chili)) 
(define a 'chili)

(subst-sauce a l)

