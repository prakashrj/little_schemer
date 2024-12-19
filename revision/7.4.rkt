#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define count-numbers
  (lambda (aexp)
    (cond
      ((null? aexp) 0)
      ((number? aexp) 1)
      (else (+ (count-numbers (car aexp)) (count-numbers (car (cdr (cdr aexp)))))))))

(define aexp1 '(1 + (3 * 4)))
(define aexp2 '((3 ^ 4) + 5))
(define aexp3 '(3 * (4 * (5 * 6))))
(define aexp4 5)
(define l1 '())
(define l2 '(3 + (66 6)))
(define lexp1 '(AND (OR x y) y))
(define lexp2 '(AND (NOT y) (OR u v)))
(define lexp3 '(OR x y))
(define lexp4 'z)

(count-numbers aexp1)
(count-numbers aexp3) 
(count-numbers aexp4)
