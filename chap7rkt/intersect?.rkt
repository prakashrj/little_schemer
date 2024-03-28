#lang racket

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      ((member (car set1) set2) #t)
      (else (intersect? (cdr set1) set2)))))

(define set1 '(stewed tomatoes and macaroni))
(define set2 '(macaroni and cheese))

(intersect? set1 set2)

