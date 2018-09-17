#lang racket

(define (accumulate f init items)
  (if (null? items)
      init
      (f (car items) (accumulate f init (cdr items)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))