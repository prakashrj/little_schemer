#lang racket

(define compose
  (lambda (f1 f2)
    (lambda (x)
      (f1 (f2 x)))))

(define square
  (lambda (x)
    (* x x)))

(define add1
  (lambda (x)
    (+ 1 x)))

(define x (compose square add1))

(x 6)

