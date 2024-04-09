#lang racket

(define o 
  (lambda (f p)
    (cond
      ((null? f) '())
      ((equal? (car (cdr p)) (car (car f))) (cons (cons (car p) (cons (car (cdr (car f))) '())) (o (cdr f) p)))
      (else (o (cdr f) p)))))

(define rem
  (lambda (l)
    (cond
      ((null? (cdr l)) (car l))
      (else (cons (car l) (rem (cdr l)))))))



(define Fcomp
  (lambda (f g)
    (cond
      ((null? g) '())
      ((null? (o f (car g))) (Fcomp f (cdr g)))
      (else (cons (rem (o f (car g))) (Fcomp f (cdr g)))))))

(define r1 '((a b) (a a) (b b)))
(define r2 '((c c)))
(define r3 '((a c) (b c)))
(define r4 '((a b) (b a)))
(define f1 '((a 1) (b 2) (c 2) (d 1)))
(define f2 '())
(define f3 '((a 2) (b 1)))
(define f4 '((1 $) (3 *)))
(define d1 '(a b))
(define d2 '(c d))
(define x 'a)

(Fcomp f1 f4)
(Fcomp f1 f3)
(Fcomp f4 f1)
(Fcomp f4 f3)



