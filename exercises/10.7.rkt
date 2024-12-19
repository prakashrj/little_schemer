#lang racket

(define atom?
(lambda (x)
(and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))


(define e1 '((lambda (x)
	   (cond
	     ((atom? x) 'done)
	     ((null? x) 'almost)
	     (else 'never)))
	 '_))

(define e2 '(((lambda (x y)
	       (lambda (u)
		 (cond (u x) (#t y)))) 1 '()) 'nil))

(define e3 '((lambda (x)
	      ((lambda (x)
		 (add1 x))
	       (add1 4)))
	    6))

(define e4 '(3 'a 'b))
(define e5 '(lambda (lat) (cons 'lat lat)))
(define e6 '(lambda (lat (lyst)) 'a 'b))

(define *lambda?
  (lambda (S)
    (cond
     ((and (equal? (first S) 'lambda) (lat? (second S))) #t)
      (else #f))))

(*lambda? e5)
(*lambda? e6)
(*lambda? e2)


