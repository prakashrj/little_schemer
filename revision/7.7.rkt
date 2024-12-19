#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((equal? a (car lat)) #t)
      (else (member? a (cdr lat))))))

(define covered?
  (lambda (lexp lat)
    (cond
      ((null? lexp) #t)
      ((atom? lexp)
       (cond
	 ((member? lexp lat) #t)
	 (else #f)))
      ((equal? 'NOT (car lexp))(covered? (car (cdr lexp)) lat))
      (else (and (covered? (car (cdr lexp)) lat) (covered? (car (cdr (cdr lexp))) lat))))))


(define aexp1 '(1 + (3 * 4)))
(define aexp2 '((3 ^ 4) + 5))
(define aexp3 '(3 * (4 * (5 * 6))))
(define aexp4 5)
(define l1 '(x y z u))
(define l2 '(3 + (66 6)))
(define lexp1 '(AND (OR x y) y))
(define lexp2 '(AND (NOT y) (OR u v)))
(define lexp3 '(OR x y))
(define lexp4 'z)

(covered? lexp1 l1)
(covered? lexp2 l1)
(covered? lexp4 l1)
