#lang racket

(define subst3
  (lambda (new o1 o2 o3 lat)
    (cond
      ((null? lat) '())
      ((or (equal? o1 (car lat)) (equal? o2 (car lat)) (equal? o3 (car lat))) (cons new (cdr lat)))
      (else
	(cons (car lat) (subst3 new o1 o2 o3 (cdr lat)))))))

(define l1 '((paella spanish) (wine red) (and beans)))
  (define l2 '())
  (define l3 '(cincinnati chili))
  (define l4 '(texas hot chili))
  (define l5 '(soy sauce and tomato sauce))
  (define l6 '((spanish) () (paella)))
  (define l7 '((and hot) (but dogs)))
    (define a1 'chili)
    (define a2 'hot)
    (define a3 'spicy)
    (define a4 'sauce)
    (define a5 'soy)
    
    
(subst3 a5 a1 a2 a4 l5)
(subst3 a4 a1 a2 a3 l4)
(subst3 a3 a1 a2 a5 l2)

