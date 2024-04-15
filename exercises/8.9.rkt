#lang racket

(define second
  (lambda (l)
    (car (cdr l))))

(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((equal?  a (car l)) #t) 
      (else (member? a (cdr l))))))

(define Rin
  (lambda (a set)
    (cond
      ((null? set) '())
      (else (cons (cons a (cons (car set) '())) (Rin a (cdr set)))))))
     
(define Rapply
  (lambda (rel x)
    (cond
      ((null? rel) '())
      ((equal? (car (car rel)) x) (cons (car (cdr (car rel))) (Rapply (cdr rel) x)))
      (else (Rapply (cdr rel) x))))) 

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (union (cdr set1) (cons (car set1) set2))))))


(define Rcomp
  (lambda (rel1 rel2)
    (cond
      ((null? rel1) '())
      (else (union (Rin (car (car rel1)) (Rapply rel2 (second (car rel1)))) (Rcomp (cdr rel1) rel2))))))

(define subst?
  (lambda (sub l)
    (cond
      ((null? sub) #t)
      ((member? (car sub) l) (subst? (cdr sub) l))
      (else #f))))


(define transitive?
  (lambda (x)
    (subst? (Rcomp x x) x)))

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
(transitive? r4)
(transitive? f1)


