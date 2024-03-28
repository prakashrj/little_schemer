#lang racket

(define addtup
 (lambda (tup)
  (cond
   ((null? tup) 0)
   (else (+ (car tup) (addtup (cdr tup)))))))

(define tup'(4 54 54 45 4 5))

(addtup tup)
   
