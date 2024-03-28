#lang racket

(define subst2
 (lambda (new o1 o2 lat)
  (cond
   ((null? lat) '())
   (else (cond
   ((eq? (car lat) o1)                                                                                
   (cons new (cdr lat)))
   ((eq? (car lat) o2)
   (cons new (cdr lat)))
   (else (subst2 new o1 o2 (cdr lat))))))))

(define new' vanilla)
(define o1' chocolate)
(define o2' banana)
(define lat'(banana ice cream with chocolate topping))

(subst2 new o1 o2 lat)
