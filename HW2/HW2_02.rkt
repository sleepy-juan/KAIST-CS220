; Principles of Programming
; HW 02 Problem 02
; 20170504 이주안

#lang racket

; accumulate: function, number, function, number, function, number -> number
; to compute the number accumulated by a way of given function(combiner)
; with a number(null-value) for base condition,
; with a function(term) for computing the value,
; with numbers(a, b) for range
; with function(next) for deteriming how to increase the value
; with iterative process
; (accumulate + 0 (lambda (x) (* 2 x)) 1 (lambda (x) (+ x 1)) 10) should produce 110
(define (accumulate combiner null-value term a next b)
  (define (iter a val)
    (if (> a b)
        val
        (iter (next a) (combiner val (term a)))))
  (iter a null-value))
; (test (accumulate + 0 (lambda (x) (* 2 x)) 1 (lambda (x) (+ x 1)) 10) 110)
; (test (accumulate * 1 (lambda (x) (* 2 x)) 1 (lambda (x) (+ x 2)) 10) 30240)