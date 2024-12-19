#lang racket

(define is-even?
  (lambda (n)
    (cond
      ((zero? n) #t)
      (else (not (is-even? (- n 1)))))))

(define is-odd?
  (lambda (n)
    (cond
      ((zero? n) #f)
      (else (not (is-odd? (- n 1)))))))

(is-even? 1)
(is-odd? 1)
