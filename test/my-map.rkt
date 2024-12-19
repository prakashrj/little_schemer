#lang racket

(define my-map
  (lambda (f l)
    (cond
      ((null? l) '())
      (else (cons (f (car l)) (my-map f (cdr l)))))))

(define add1
  (lambda (n)
    (+ 1 n)))

(my-map add1 '(1 2 3 4 5))
