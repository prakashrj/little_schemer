#lang racket

(define Rapply
  (lambda (rel x)
    (cond
      ((null? rel) '())
      ((equal? (car (car rel)) x) (cons (car (cdr (car rel))) (Rapply (cdr rel) x)))
      (else (Rapply (cdr rel) x)))))

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

(Rapply f1 x)
(Rapply r1 x)
(Rapply f2 x)


