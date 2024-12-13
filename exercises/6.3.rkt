#lang racket 

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define double*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
	 ((equal? (car l) a)
	  (cons (car l) (cons (car l) (double* a (cdr l)))))
       (else (cons (car l) (double* a (cdr l))))))
      (else (cons (double* a (car l)) (double* a (cdr l)))))))

(define l1 '((fried potatoes) (baked (fried)) tomatoes))
(define l2 '(((chili) chili (chili))))
(define l3 '())
(define lat1 '(chili and hot))
(define lat2 '(baked fried))
(define a 'fried)

(double* a l1)
(double* a l2)
(double* a l3)


     
