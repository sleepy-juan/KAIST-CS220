#lang racket

(define (deep-reverse items)
  (if (null? items)
      empty
      (if (pair? (car items))
          (append (deep-reverse (cdr items)) (list (deep-reverse (car items))))
          (append (deep-reverse (cdr items)) (list (car items))))))

