#lang racket

(define map
  (lambda (f l)
    (cond
      ((null? l) '())
      (else (cons (f (car l)) (map f (cdr l)))))))
