#lang racket

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
	 ((eq? (car l) old)
	  (cons new
		(cons old
		   (insertL* new old
			     (cdr l)))))
	 (else (cons (car l)
		     (insertL* new old
			       (cdr l))))))
  (else (cons (insertL* new old
			(car l))
	      (insertL* new old 
			(cdr l)))))))
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define new 'pecker)
(define old 'chuck)
(define l '((how much (wood))
	    could
	    ((a (wood) chuck))
	    (((chuck)))
	    (if (a) ((wood chuck)))
	    could chuck wood))


(insertL* new old l)
