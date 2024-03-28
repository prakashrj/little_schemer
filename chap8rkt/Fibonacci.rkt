#lang racket

(define Fibonacci
  (lambda (n)
    (cond
      ((eq? n 1) 0)
      ((eq? n 2) 1)
      (else (+ (Fibonacci (- n 1)) (Fibonacci (- n 2)))))))


(Fibonacci 50)
  
