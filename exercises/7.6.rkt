#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define member?
  (lambda (a lat)
    (cond 
     ((null? lat) #f)
     ((equal? a (car lat)) #t)
     (else (member? a (cdr lat))))))

(define v '(AND OR NOT))
(define a '(AND))
(define b '(OR))
(define c '(NOT))

(define and-exp?
  (lambda (lexp)
    (cond
      ((null? lexp) #f)
      ((atom? lexp) #t)
      ((null? (cdr lexp))
       (cond
	 ((atom? (car lexp)) #t)
         ((list? (car lexp)) (and-exp? (car lexp)))
	 (else #f)))
      ((member? (car lexp) a) 
       (cond
         ((atom? (car (cdr lexp))) (and-exp? (cdr (cdr lexp))))
         ((list? (car (cdr lexp))) (and (and-exp? (car (cdr lexp))) (and-exp? (cdr (cdr lexp)))))
         (else #f)))
      (else #f))))

(define or-exp?
  (lambda (lexp)
    (cond
      ((null? lexp) #f)
      ((atom? lexp) #t)
      ((null? (cdr lexp))
       (cond
	 ((atom? (car lexp)) #t)
         ((list? (car lexp)) (or-exp? (car lexp)))
	 (else #f)))
      ((member? (car lexp) b) 
       (cond
         ((atom? (car (cdr lexp))) (or-exp? (cdr (cdr lexp))))
         ((list? (car (cdr lexp))) (and (or-exp? (car (cdr lexp))) (or-exp? (cdr (cdr lexp)))))
         (else #f)))
      (else #f))))
      
(define not-exp?
  (lambda (lexp)
    (cond
      ((null? lexp) #f)
      ((atom? lexp) #t)
      ((null? (cdr lexp))
       (cond
	 ((atom? (car lexp)) #t)
         ((list? (car lexp)) (not-exp? (car lexp)))
	 (else #f)))
      ((member? (car lexp) c) 
       (cond
         ((atom? (car (cdr lexp))) (not-exp? (cdr (cdr lexp))))
         ((list? (car (cdr lexp))) (and (not-exp? (car (cdr lexp))) (not-exp? (cdr (cdr lexp)))))
         (else #f)))
      (else #f))))

(define lexp?
  (lambda (lexp)
    (cond
      ((null? lexp) #f)
      ((atom? lexp) #t)
      ((null? (cdr lexp))
       (cond
	 ((atom? (car lexp)) #t)
         ((list? (car lexp)) (lexp? (car lexp)))
	 (else #f)))
      ((member? (car lexp) v) 
       (cond
         ((atom? (car (cdr lexp))) (lexp? (cdr (cdr lexp))))
         ((list? (car (cdr lexp))) (and (lexp? (car (cdr lexp))) (lexp? (cdr (cdr lexp)))))
         (else #f)))
      (else #f))))



(define aexp1 '(1 + (3 * 4)))
(define aexp2 '((3 ↑ 4) + 5))
(define aexp3 '(3 * (4 * (5 * 6))))
(define aexp4 '5)
(define l1 '())
(define l2 '(3 + (66 6)))
(define lexp1 '(AND (OR x y) y))
(define lexp2 '(AND (OR x y) y))
(define lexp3 '(OR x y))
(define lexp4 'z)

(lexp? lexp1)
(lexp? lexp2)
(lexp? lexp3)
(lexp? lexp4)
(lexp? l2)
(lexp? l1)
(lexp? aexp1)
(lexp? aexp2)
(lexp? aexp3)

(and-exp? lexp1)
(and-exp? lexp2)
(and-exp? lexp3)
(and-exp? lexp4)

(or-exp? lexp1)
(or-exp? lexp2)
(or-exp? lexp3)
(or-exp? lexp4)

(not-exp? lexp1)
(not-exp? lexp2)
(not-exp? lexp3)
(not-exp? lexp4)





