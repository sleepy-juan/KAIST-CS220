; Principles of Programming
; HW 02 Problem 04
; 20170504 이주안

#lang racket

; double: function -> function
; to return the function which is applied the given function twice.
; (double (lambda (x) (* x x))) should produce a function that computes x^4
(define (double f) (lambda (x) (f (f x))))
; (test ((double (lambda (x) (* x x))) 2) 16)


; (define (inc x) (+ x 1))
; (((double (double double)) inc) 5)
; above code returns 21