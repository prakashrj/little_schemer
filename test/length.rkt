#lang racket

(define length
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (+ 1 (length (cdr l)))))))

(length '(a b))
