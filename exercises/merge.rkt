#lang racket

(define cons2
  (lambda (l1 l2)
  (cond
  ((null? l2) l1)
  (else (cons2 (append l1 (car l2)) (cdr l2))))))

  


(define Merge
(lambda (l1 l2 v)
    (cond
      ((null? l1) (append v l2))
      ((null? l2) (append v l1))
    ((< (car l1) (car l2))
   (Merge (cdr l1) l2 (append v (car l1))))
    (else (Merge l1 (cdr l2) (append v (car l2)))))))

(define l1 '(1 3 5))
(define l2 '(2 4 6))


(Merge l1 l2 '()) 
