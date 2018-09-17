; Principles of Programming
; HW 02 Problem 01
; 20170504 이주안

#lang racket

; product: function, number, function, number -> number
; to compute the product of a given function(term) of a number which is in the range of [a,b]
; where the number increases with the way of a given function(next)
; (product (lambda (x) x) 1 (lambda (x) (+ x 1)) 5) should produce 120
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))
; (test (product (lambda (x) x) 1 (lambda (x) (+ x 1)) 2) 2)
; (test (product (lambda (x) x) 1 (lambda (x) (+ x 1)) 3) 6)
; (test (product (lambda (x) x) 1 (lambda (x) (+ x 1)) 4) 24)

; factorial: number -> number
; to compute the factorial of given number
; (factorial 5) should produce 120
(define (factorial n) (product (lambda (x) x) 1 (lambda (x) (+ x 1)) n))
; (test (factorial 2) 2)
; (test (factorial 3) 6)
; (test (factorial 4) 24)

; pi-product: number -> number
; to compute pi with the given equation with a number n
; (pi-product 1000) should produce 3.1431607055322663
(define (pi-product n)
  (* 4.0
  (/ (product (lambda (x) (+ 2 (* 2 (floor (/ x 2))))) 1 (lambda (x) (+ x 1)) n)
     (product (lambda (x) (+ 1 (* 2 (ceiling (/ x 2))))) 1 (lambda (x) (+ x 1)) n))))
; (test (pi-product 10) 3.2751010413348074)
; (test (pi-product 100) 3.1570301764551676
; (test (pi-product 1000) 3.1431607055322663)

; problem 1 - b
; I didn't have to do it, but... I did it

; iterative-product function, number, function, number -> number
; to do same process as function product, with iterative method
; (iterative-product (lambda (x) x) 1 (lambda (x) (+ x 1)) 5) should produce 120
(define (iterative-product term a next b)
  (define (iter-product a val)
    (if (> a b)
        val
        (iter-product (next a) (* val (term a)))))
  (iter-product a 1))
; (test (iterative-product (lambda (x) x) 1 (lambda (x) (+ x 1)) 2) 2)
; (test (iterative-product (lambda (x) x) 1 (lambda (x) (+ x 1)) 3) 6)
; (test (iterative-product (lambda (x) x) 1 (lambda (x) (+ x 1)) 4) 24)