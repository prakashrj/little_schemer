#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define firsta
  (lambda (alist)
    (cond
      ((null? alist) '())
      ((atom? (car alist)) (car alist))
      ((null? (car alist))(firsta (cdr alist)))
      (else (firsta (car alist))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((equal? (car lat) a)(multirember a (cdr lat)))
       (else (cons (car lat) (multirember a (cdr lat)))))))

(define multirember*
  (lambda (a alist)
    (cond
      ((null? alist) '())
      (else (cons (multirember a (car alist)) (multirember* a (cdr alist)))))))

(define domset
  (lambda (r)
    (cond
      ((null? r) '())
      ((null? (car r))(domset (cdr r)))
      (else (cons (firsta r) (domset (multirember* (firsta r) r)))))))

(define length
  (lambda (lat)
    (cond
      (( null? lat) 0)
      (else (+ 1 (length (cdr lat)))))))

(define reflexive?
  (lambda (r)
    (= (length r) (- (expt 2 (length (domset r))) 1))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((equal? a (car lat) #t)
       (else (member? a (cdr lat)))))))



(define v '())

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


(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

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

(define quasi-order
  (lambda (x)
    (and (reflexive? x) (transitive? x))))

(define partial-order
  (lambda (x)
    (and (quasi-order x) (antisymmetric? x))))

(define equivalence
  (lambda (x)
    (and (quasi-order x) (symmetric? x))))
