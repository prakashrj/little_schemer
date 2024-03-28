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
      ((equal? var (car (car al)))
       (cond
	 ((zero? (car (cdr (car al)))) #f)
	 (else #t)))
      (else (lookup var (cdr al))))))

(define Mlexp
  (lambda (lexp al)
    (cond
     ((atom? lexp) (lookup lexp al))
     ((equal? 'AND (car lexp))
      (cond
	((equal? '(AND) lexp) #t)
	((atom? (car (cdr lexp)))
	 (cond
	   ((equal? #t (lookup (car (cdr lexp)) al)) (Mlexp (cons 'AND (cdr (cdr lexp))) al))
	   (else #f)))
	(else (and (Mlexp (car (cdr lexp)) al) (Mlexp (cons 'AND (cdr (cdr (cdr lexp)))) al)))))
     ((equal? 'OR (car lexp))
      (cond
	((equal? '(OR) lexp) #f)
	((atom? (car (cdr lexp)))
	 (cond
	   ((equal? #f (lookup (car (cdr lexp)) al)) (Mlexp (cons 'OR (cdr (cdr lexp))) al))
	   (else #t)))
	(else (and (Mlexp (car (cdr lexp)) al) (Mlexp (cons 'OR (cdr (cdr (cdr lexp)))) al)))))
     ((equal? #f  (covered? lexp (get! al))) 'not-covered)
     (else (not (Mlexp (car (cdr lexp)) al))))))

(define l1 '((x 1) (y 0) (z 0)))
(define l2 '((y 0) (u 0) (v 1)))
(define lexp1 '(AND (OR x y) (Or x z) y))
(define lexp2 '(AND (NOT y) (OR u v) v u))
(define lexp4 'z)

(Mlexp lexp1 l1)
(Mlexp lexp2 l2)
(Mlexp lexp4 l1)
