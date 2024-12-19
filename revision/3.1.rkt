#lang racket

(define seconds
  (lambda (l)
    (cond
      ((null? l) '())
      (else
      (cons (car (cdr (car l))) (seconds (cdr l)))))))

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

    (seconds l1)
    (seconds l2)
    (seconds l7)


    
    
