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
  (lambda (lexp los)
    (cond
      ((atom? lexp) 
       (cond
	 ((member? lexp los) #t)
	 (else #f)))
      ((null? lexp) #t)
      ((null? (cdr lexp))
       (cond
	 ((list? (car lexp))
	 (covered? (car lexp) los))
	 (else
	   (cond
	     ((member? (car lexp) los) #t)
	     (else #f)))))
      ((null? (cdr (cdr lexp)))
	(cond 
	  ((list? (car (cdr lexp))) (covered? (car (cdr lexp)) los)) 
	  (else 
	    (cond 
	      ((member? (car (cdr lexp)) los) #t) 
	      (else #f))))) 
       ((list? (car (cdr lexp))) 
	     (cond 
	    ((list? (car (cdr (cdr lexp)))) (and (covered? (car (cdr lexp)) los) (covered? (car (cdr (cdr lexp))))))
	    (else (and (covered? (car (cdr lexp)) los) (member? (car (cdr (cdr lexp))) los)))))
       (else 
	 (cond
	 ((list? (car (cdr (cdr lexp)))) (and (covered? (car (cdr (cdr lexp))) los) (member? (car (cdr lexp)) los))) 
	     (else (and (member? (car (cdr (cdr lexp))) los) (member? (car (cdr lexp)) los))))))))

(define aexp1 '(1 + (3 * 4)))
(define aexp2 '((3 ↑ 4) + 5))
(define aexp3 '(3 * (4 * (5 * 6))))
(define aexp4 ' 5)
(define l1 '())
(define l2 '(3 + (66 6)))
(define lexp1 '(AND (OR x y) y))
(define lexp2 '(AND (NOT y) (OR u v)))
(define lexp3 '(OR x y))
(define lexp4 'z)

(covered? lexp1 '(x y z u))
(covered? lexp3 '(x y z u)) 
(covered? lexp4 '(x y z u)) 
(covered? lexp2 '(x y z u)) 

