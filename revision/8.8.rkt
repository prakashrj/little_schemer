#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define Rapply
  (lambda (rel x)
    (cond
      ((null? rel) '())
      ((equal? (car (car rel)) x)(cons (car (cdr (car rel))) (Rapply (cdr rel) x)))
      (else (Rapply (cdr rel) x)))))

(define Rin
  (lambda (x set)
    (cond
      ((null? set) '())
      (else (cons (cons x (cons (car set) '())) (Rin x (cdr set)))))))

(define Rcomp
  (lambda (rel1 rel2 )
    (cond
      ((null? rel1) '())
      (else (cons (Rin (first (car rel1)) (Rapply rel2 (second (car rel1)))) (Rcomp (cdr rel1) rel2))))))

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

(Rcomp r1 r3)
(Rcomp r1 f1)
(Rcomp r1 r1)



