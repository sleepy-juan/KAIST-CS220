; Principles of Programming
; HW 02 Problem 03
; 20170504 이주안

#lang racket

; criterion determining whether the guess is close enough to real value
(define tolerance 0.00001)

; fixed-point: function number -> number
; to compute the fixed point of f
; (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) should produce 1.6180327868852458
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
; (test (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) 1.6180327868852458)
; (test (fixed-point (lambda (x) (+ 1 (/ 1 x))) 2.0) 1.6180327868852458)
; (test (fixed-point (lambda (x) (+ 2 (/ 2 x))) 1.0) 2.732052578361982)

; compute the golden ratio with fixed-point
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)