#lang racket

(provide (all-defined-out))

(define (transpose xss)
  (apply map list xss))

(define (log10 x)
  (/ (log x)
     (log 10))
  )

(define (remove*_pos element l function)
  (remove* (list element) l
           (lambda(e in_l)
             (equal? e
                     (function in_l)))
           )
  )