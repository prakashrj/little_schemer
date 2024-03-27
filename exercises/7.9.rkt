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
	    ((list? (car (cdr (cdr lexp)))) (and (covered? (car (cdr lexp)) los) (covered? (car (cdr (cdr lexp))) los)))
	    (else (and (covered? (car (cdr lexp)) los) (member? (car (cdr (cdr lexp))) los)))))
       (else 
	 (cond
	 ((list? (car (cdr (cdr lexp)))) (and (covered? (car (cdr (cdr lexp))) los) (member? (car (cdr lexp)) los))) 
	     (else (and (member? (car (cdr (cdr lexp))) los) (member? (car (cdr lexp)) los))))))))

(define get!
  (lambda (al)
    (cond
      ((null? al) '())
      (else (cons (car (car al)) (get! (cdr al)))))))
      
(define lookup
  (lambda (var al)
    (cond
      ((null? al) '?)
      ((member? (car (car al)) var) (car (cdr (car al))))
      (else (lookup var (cdr al))))))

(define pair=
  (lambda (al)
    (cond
      ((null? al) '())
      ((zero? (car (cdr (car al)))) (define (car (car al)) #f) (pair= (cdr al)))
      (else (define (car (car al)) #f) (pair= (cdr al)))

(define Mlexp
  (lambda (lexp l1)
    (cond


(define l1 '((x 1) (y 0) (z 0)))
(define l2 '((y 0) (u 0) (v 1)))
(define lexp1 '(AND (OR x y) y))
(define lexp2 '(AND (NOT y) (OR u v)))
(define lexp4 'z)

;(Mlexp lexp1 l1)
;(Mlexp lexp2 l2)
(Mlexp lexp4 l1)
