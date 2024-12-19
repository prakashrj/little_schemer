#lang racket

(define value
  (lambda (aexp)
    (cond
      ((number? aexp) aexp)
      ((eq? '+ (car aexp)) (+ (value (second aexp)) (value (third aexp))))
      ((eq? '* (car aexp)) (* (value (second aexp)) (value (third aexp)))))))

(value '(* 8 (+ 1 2)))
