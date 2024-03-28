#lang racket

(define firsts
 (lambda (l)
  (cond
   ((null? l) '())
   (else (cons (car (car l))
   (firsts (cdr l)))))))

(define l'((a c e) (b c e) (c c e)))

(firsts l)
