#lang racket

(define subset
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member (car set1) set2)
       (subset (cdr set1) set2))
      (else #f))))

(define set1 '(4 pounds of horseradish))
(define set2 '(4 pounds chicken and 5 ounces horse radish))

(subset set1 set2)
