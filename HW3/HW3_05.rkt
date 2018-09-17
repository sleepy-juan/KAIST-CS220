#lang racket

(define (fold-right f init items)
  (if (null? items)
      init
      (f (car items) (fold-right f init (cdr items)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest)) (cdr rest))))
    (iter initial sequence))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list empty (list 1 2 3))
(fold-left list empty (list 1 2 3))

; op should be able to take two elements