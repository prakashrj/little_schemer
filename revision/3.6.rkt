#lang racket

(define member?
  (lambda (a lat)
    (cond
    ((null? lat) #f)
    ((equal? (car lat) a) #t)
    (else (member? a (cdr lat))))))

(define substN
  (lambda (new slat lat)
    (cond
    ((null? lat) '())
    ((member? (car lat) slat) (cons new (cdr lat)))
    (else
      (cons (car lat) (substN new slat (cdr lat)))))))
  

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
    
   (substN a2 l3 l4)
   (substN a4 l3 l5)
   (substN a4 l3 l2)
