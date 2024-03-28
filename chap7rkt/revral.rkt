#lang racket

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (cons
		    (car (cdr (car rel)))
		    (cons (car (car rel))
			  (quote ())))
		  (revrel (cdr rel)))))))

(define rel '((8 a) (pumpkin pie) (got sick)))

(revrel rel)
