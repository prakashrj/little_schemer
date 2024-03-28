#lang racket

(define insertL
 (lambda (new old lat)
  (cond
    ((null? lat) '())
    (else (cond
      ((eq? (car lat) old)
       (cons new 
	 (cons old (cdr lat))))
      (else (cons (car lat) 
	      (insertL new old 
		(cdr lat)))))))))

(define lat '(ice cream with fudge for dessert))
(define new 'topping)
(define old 'fudge)

(insertL new old lat)

