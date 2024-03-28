#lang racket

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else
	(cond
	  ((eq? (car lat) a)
	   (multirember a (cdr lat)))
	  (else (cons (car lat)
		      (multirember a
				   (cdr lat)))))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
	(cons (car lat) (makeset (multirember (car lat) (cdr lat))))))))

(define lat' (apple apple hello what peach what peach))

(makeset lat)
