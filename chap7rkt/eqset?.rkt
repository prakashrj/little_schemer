#lang racket

(define subset
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member (car set1) set2)
       (subset (cdr set1) set2))
      (else #f))))

(define eqset?
  (lambda (set1 set2)
   (and (subset set1 set2)
	 (subset set2 set1)))) 

(define set1 '(6 large chickens with wings))
(define set2 '(6 chickens with large wings))

(eqset? set1 set2)

