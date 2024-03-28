#lang racket

(define all-nums
 (lambda (lat)
  (cond
 ((null? lat) '())
 (else (cond
 ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
 (else (all-nums (cdr lat))))))))

(define lat '( 87 nine 34 four))

(all-nums lat)
