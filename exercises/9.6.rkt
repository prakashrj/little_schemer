#lang racket

(define accum
  (lambda (op lat e)
    (cond
      ((null? lat) e)
      (else (op (car lat) (accum op (cdr lat) e))))))

(define addvec
  (lambda (lat)
    (accum + lat 0)))

(define multvec
  (lambda (lat)
    (accum * lat 1)))

(define length
  (lambda (lat)
    (accum (lambda (x y) (+ 1 y)) lat 0)))

(define occur
  (lambda (a lat)
    (accum (lambda (x y) (cond ((eq? x a) #t) (else y))) lat #f)))

(define lat '(1 2 3 2 4 3 4 1 3 1 2 4))

(length lat)
(addvec lat)
(multvec lat)
(occur 4 lat)

