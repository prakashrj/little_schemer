#lang racket

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((equal? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))



(define rember2
  (lambda (a l)
    (cond
      ((null? l) '())
      ((equal? (car l) a) (cons (car l) (rember a (cdr l))))
      (else (cons (car l) (rember2 a (cdr l))))))) 

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
    
   (rember2 a1 l3)
   (rember2 a4 l5)
   (rember2 a4 l2)
