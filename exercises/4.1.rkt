#lang racket

(define duplicate
  (lambda (n obj)
    (cond
    ((zero? n) '())
 (else (cons obj (duplicate (- n 1) obj))))))

(define n 2)
(define obj 'Hello)

(duplicate n obj) 

