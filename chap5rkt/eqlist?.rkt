#lang racket

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2))
       (= a1 a2))
      ((or (number? a1) (number? a2))
       #f)
      (else (eq? a1 a2)))))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define eqlist?
  (lambda (l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f)
    ((and (atom? (car l1))
	  (atom? (car l2)))
     (and (eqan? (car l1) (car l2))
	  (eqlist? (cdr l1) (cdr l2))))
  ((or (atom? (car l1))
       (atom? (car l2)))
   #f)
  (else
    (and (eqlist? (car l1) (car l2))
	 (eqlist? (cdr l1) (cdr l2)))))))

(define l1 '(Hello))

(define l2 '(Hello))

(eqlist? l1 l2)
