#lang racket

(define build
  (lambda (s1 s2)
    (cond
      (else (cons s1 s2 '())))))

(define first
  (lambda (p)
    (cond (else (car p)))))

(define second 
  (lambda (p)
    (cond (else (car (cdr p))))))
