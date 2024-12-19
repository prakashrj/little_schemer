#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define op?
  (lambda (a)
    (cond
      ((equal? a '+) #t)
      ((equal? a '*) #t)
      ((equal? a '^) #t)
      (else #f))))

(define aexp?1
  (lambda (l)
    (cond
      ((null? l) #t)
      ((number? l) #t)
      ((op? (car (cdr l)))(and (aexp?1 (car l)) (aexp?1 (car (cdr (cdr l))))))
      (else #f))))

(define aexp?
  (lambda (l)
    (and (aexp?1 l) (not (null? l)))))


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

(aexp? aexp1)
(aexp? aexp2)
(aexp? l1)
(aexp? l2)
