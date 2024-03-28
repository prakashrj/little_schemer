#lang racket

(define insertR
 (lambda (new old lat)
  (cond
    ((null? lat) '())
    (else (cond
      ((eq? (car lat) old)
       (cons old 
	 (cons new (cdr lat))))
      (else (cons (car lat) 
	      (insertR new old 
		(cdr lat)))))))))

(define lat '(ice cream with fudge for dessert))
(define new 'topping)
(define old 'fudge)

(insertR new old lat)

