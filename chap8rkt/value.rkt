#lang racket

(define atom-function
  (lambda (x)
    (cond
    ((equal? x  '+) +)
    ((equal? x '*) *)
    (else ↑))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))
(define operator
  (lambda (aexp)
    (car eaxp)))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else 
	((atom-to-function
	   (operator nexp))
	 (value (1st-sub-exp nexp))
	 (value (2nd-sub-exp nexp)))))))


