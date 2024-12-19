#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((equal? (car lat) a) #t)
      (else (member? a (cdr lat))))))

(define Rapply
  (lambda (rel x)
    (cond
      ((null? rel) '())
      ((equal? (car (car rel)) x)(cons (car (cdr (car rel))) (Rapply (cdr rel) x)))
      (else (Rapply (cdr rel) x)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)(union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

(define Rin
  (lambda (x set)
    (cond
      ((null? set) '())
      (else (cons (cons x (cons (car set) '())) (Rin x (cdr set)))))))

(define Rcomp
  (lambda (rel1 rel2 )
    (cond
      ((null? rel1) '())
      (else (union (Rin (first (car rel1)) (Rapply rel2 (second (car rel1)))) (Rcomp (cdr rel1) rel2))))))

(define subset?
  (lambda (set sbset)
    (cond
      ((null? sbset) #t)
      ((member? (car sbset) set)(subset? set (cdr sbset)))
      (else #f))))

      

(define transitive?
  (lambda (r)
    (subset? r (Rcomp r r))))

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

(transitive? r1)
(transitive? r3) 
(transitive? f1)

