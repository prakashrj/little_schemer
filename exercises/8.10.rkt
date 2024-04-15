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

(define ab
  (lambda (al v)
    (cond
      ((null? al) v)
      ((equal? (car (car al)) (car (cdr (car al)))) (ab (cdr al) (cons (car (car al)) v)))
      (else (ab (cdr al) v)))))

(define reflexive?
  (lambda (al)
    (cond
      ((null? al) #t)
      ((member? (car (car al)) (ab al '()))
       (cond
	 ((member? (car (cdr (car al))) (ab al '())) (reflexive? (cdr al)))
	 (else #f)))
      (else #f))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
	      ((eq? (car lat) a)
	       (multirember a (cdr lat)))
	    (else (cons (car lat)
			(multirember a
				     (cdr lat)))))))

(define reverselat
  (lambda (l v)
    (cond
      ((null? l) v)
      (else (reverselat (cdr l) (cons (car l) v))))))



(define symmetric?
  (lambda (rel)
    (cond
      ((null? rel) #t)
      ((member? (reverselat (car rel) '()) rel) (symmetric? (multirember  (car rel) (multirember (reverselat (car rel) '()) rel))))
      (else #f))))

(define antisymmetric?
  (lambda (rel)
    (cond
      ((null? rel) #t)
      ((equal? (car (car rel)) (car (cdr (car rel)))) (antisymmetric? (cdr rel)))
      ((member? (reverselat (car rel) '()) rel) #f)
      (else (antisymmetric? (multirember  (car rel) (multirember (reverselat (car rel) '()) rel)))))))

(define quasi-order?
  (lambda (l)
    (and (transitive? l) (reflexive? l))))

(define partial-order?
  (lambda (l)
    (and (quasi-order? l) (antisymmetric? l))))

(define equivalence?
  (lambda (l)
    (and (quasi-order? l) (symmetric? l))))


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

(quasi-order? r1)
(partial-order? r1)
(equivalence? r1)



