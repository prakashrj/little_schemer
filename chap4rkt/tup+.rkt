#lang racket

(define tup+
 (lambda (tup1 tup2)
  (cond
  ((null? tup1) tup2)
  ((null? tup2) tup1)
  (else
    (cons (+ (car tup1) (car tup2))
    (tup+ (cdr tup1) (cdr tup2)))))))

(define tup1 '( 34 34 34 34))
(define tup2 '( 34 34 34 34))

(tup+ tup1 tup2)
