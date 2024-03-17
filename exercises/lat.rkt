(define lat?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((atom? (car lat))
       (lat? (cdr lat)))
      (else #f))))
