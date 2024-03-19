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

(define v '(+ * ↑))

(define aexp?
  (lambda (aexp)
    (cond
      ((null? aexp) #f)
      ((number? aexp) #t)
      ((null? (cdr aexp))
       (cond
	 ((number? (car aexp)) #t)
         ((list? (car aexp)) (aexp? (car aexp)))
	 (else #f)))
      ((member? (car (cdr aexp)) v) 
       (cond
         ((number? (car aexp)) (aexp? (cdr (cdr aexp))))
         ((list? (car aexp)) (and (aexp? (car aexp)) (aexp? (cdr (cdr aexp)))))
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

(aexp? l1)
(aexp? l2)
(aexp? aexp1)
(aexp? aexp2)
(aexp? aexp3)
(aexp? aexp4)
