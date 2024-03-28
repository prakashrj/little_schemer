#lang racket

(define duplic
  (lambda (n l)
    (cond
      ((zero? n) '())
      (else (cons l (duplic (- n 1) l))))))

(define n 3)
(define l '(x y))

(duplic 3 l)
	       
