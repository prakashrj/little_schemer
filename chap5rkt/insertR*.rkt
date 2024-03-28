#lang racket

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
	 ((eq? (car l) old)
	  (cons old
		(cons new
		   (insertR* new old
			     (cdr l)))))
	 (else (cons (car l)
		     (insertR* new old
			       (cdr l))))))
  (else (cons (insertR* new old
			(car l))
	      (insertR* new old 
			(cdr l)))))))
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define new 'roast)
(define old 'chuck)
(define l '((how much (wood))
	    could
	    ((a (wood) chuck))
	    (((chuck)))
	    (if (a) ((wood chuck)))
	    could chuck wood))


(insertR* new old l)


