#lang racket

(define no-numbs
 (lambda (lat)
 (cond 
 ((null? lat) '())
 (else
 (cond ((number? (car lat)) (no-numbs (cdr lat)))
 (else (cons (car lat) (no-numbs (cdr lat)))))))))

(define lat'( 0 peach 12 dollars and 89 watermelons))

(no-numbs lat)
