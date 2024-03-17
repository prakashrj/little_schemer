#lang racket

(define seconds
  (lambda (l)
    (cond
      ((null? l) '())
      (else
	(cons (car (cdr (car l)))
	      (seconds (cdr l)))))))

(define l '((paella spanish) (wine red) (and beans)))

(seconds l)
