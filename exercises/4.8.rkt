#lang racket

(define /
  (lambda (n m)
    (cond
      ((> n m) (+ 1 (/ (- n m) m)))
      ((= n m) 1)
      (else 0))))

(/ 56 7)


