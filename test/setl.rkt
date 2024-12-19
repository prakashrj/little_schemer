#lang racket

(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((equal? a (car l)) #t)
      (else (member? a (cdr l))))))

(define setl1
  (lambda (l1 l2)
    (cond
      ((null? l1) l2)
      ((member? (car l1) l2) (setl1 (cdr l1) l2))
      (else (setl1 (cdr l1) (cons (car l1) l2))))))

(define setl
  (lambda (l)
    (setl1 l '())))

(setl '(a b d c d a b d c a b d c d d d))
