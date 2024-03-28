#lang racket

(define occur
 (lambda (a lat)
  (cond 
  ((null? lat) 0)
  (else (cond
  ((eq? (car lat) a) (+ 1 (occur a (cdr lat))))
  (else (occur a (cdr lat))))))))

(define lat '(Hello world Hello you))
(define a 'Hello)
 
(occur a lat)
