#lang racket

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member (car set1) set2)
       (cons (car set1)
	     (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2))))) 

(define set1 '(stewed tomatoes and 
		      macaroni casserole))

(define set2 '(macaroni and cheese))
(intersect set1 set2)
