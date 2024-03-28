#lang racket

(define dup
  (lambda (a l)
    (cond
      ((null? l) '())
      ((eq? (car l) a) 
       (cons a (cons a (cdr l))))
    (else (cons (car l) (dup a (cdr l)))))))

(define l '( Hello with pillow))
(define a 'with)

(dup a l)

