#lang racket

(define toh
  (lambda (n)
    (cond
      ((eq? n 1) 1)
      (else (+ (* 2 (toh (- n 1))) 1)))))

(define n 3)

(toh 3)
