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

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
       (eqan? s1 s2))
      ((atom? s1) #f)
      ((atom? s2) #f)
      (else (eqlist? s1 s2)))))

(define s1 '(Hello world))
(define s2 '(Hello world))

(equal? s1 s2)
