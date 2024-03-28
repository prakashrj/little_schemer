#lang racket

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))
