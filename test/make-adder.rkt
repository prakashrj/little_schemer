#lang racket

(define make-adder
  (lambda (n)
    (lambda (x)
      (+ x n))))

(define add5 (make-adder 5))

(add5 1)
