#lang racket

(define !
  (lambda (n)
    (cond
      ((eq? n 1) 1)
      (else (* n (! (- n 1)))))))

(! 6)
