#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define count-op-f
  (lambda (a? aexp)
    (cond
      ((null? aexp) 0)
      ((atom? aexp)
       (cond
	 ((a? aexp) 1)
	 (else 0)))
      (else (+ (count-op-f a? (car aexp)) (count-op-f a? (cdr aexp)))))))

(define +?
  (lambda (a)
    (equal? '+ a)))

(define ^?
  (lambda (a)
    (equal? '^ a)))

(define *?
  (lambda (a)
    (equal? '* a)))

(define op?
  (lambda (a)
    (or (equal? '* a) (equal? '+ a) (equal? '^ a))))

(

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

(count-op aexp1) 
(count-op aexp3)
(count-op aexp4)
