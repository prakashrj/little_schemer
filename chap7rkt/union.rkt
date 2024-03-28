#lang racket

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member (car set1) set2)
       (union (cdr set1) set2))
      (else (cons (car set1)
		  (union (cdr set1) set2))))))

(define set1 '(stewed tomatoes and
		      macaroni casserole))
(define set2 '(macaroni and cheese))

(union set1 set2)
