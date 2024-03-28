#lang racket

(define 1st-sub-exp
  (lambda (aexp)
    (cdr (car aexp))))
