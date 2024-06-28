#lang racket

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
		(member? a (cdr lat)))))))

(define or-func
  (lambda (or1 or2)
    (or (or1) (or2))))

(define and-func
  (lambda (and1 and2)
    (and (and1) (and2))))

(define set-f?
  (lambda (logical? const)
    (lambda (set1 set2)
      (cond ((null? set1) const)
	    (else (logical?
		    (lambda () (member? (car set1) set2))
		    (lambda () ((set-f? logical? const) (cdr set1) set2))))))))

(define intersect? (set-f? or-func #f))
(define subset? (set-f? and-func #t))

(define set1 '(4 pounds of horseradish))
(define set2 '(four pounds chicken and 5 ounces horseradish))
(define set3 '(stewed tomatoes and macaroni))
(define set4 '(macaroni and cheese))

(intersect? set3 set4)
(subset? set1 set2)
