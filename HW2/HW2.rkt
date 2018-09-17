; Principles of Programming
; 20170504 이주안

#lang racket

; HW 02 Problem 01

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



; HW 02 Problem 02

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



; HW 02 Problem 03

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
; (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)



; HW 02 Problem 04

; double: function -> function
; to return the function which is applied the given function twice.
; (double (lambda (x) (* x x))) should produce a function that computes x^4
(define (double f) (lambda (x) (f (f x))))
; (test ((double (lambda (x) (* x x))) 2) 16)

; (define (inc x) (+ x 1))
; (((double (double double)) inc) 5)
; above code returns 21



; HW 02 Problem 05

; square: number -> number
; to compute the square of given number
; (square 3) should produce 9
(define (square x) (* x x))
; (test (square 3) 9)

; Problem a

; sqrt-of-square: number -> boolean
; to determine whether the given value is equal to the square root of square of value
; (sqrt-of-square 4) should produce #t
(define (sqrt-of-square x)
  (= x (sqrt (square x))))
; (test (sqrt-of-square 4.1) #t)

; Problem b

; square-of-sqrt: number -> boolean
; to determine whether the given value is equal to the square of square root of value
; (square-of-sqrt 4) should produce #t
; However, (square-of-sqrt 5) should produce #f
; because square root makes some error in computation.
(define (square-of-sqrt x)
  (= x (square (sqrt x))))
; (test (square-of-sqrt 4) #t)
; (test (square-of-sqrt 5) #f)

; Problem c

; inverse-test: function, function, number -> boolean
; to determine whether the given value is equal to the f1 of f2 of value
; (inverse-test square sqrt 5.0) should produce #f
; (inverse-test sqrt square 5.0) should produce #t
(define (inverse-test f1 f2 value)
  (= value (f1 (f2 value))))
; (test (inverse-test square sqrt 5.0) #f)
; (test (inverse-test sqrt square 5.0) #t)

; Problem d

; make-inverse-test: function, function -> function
; to return the inverse-test of given two functions with the order
; (make-inverse-test square sqrt) should return the function same as square-of-sqrt
(define (make-inverse-test f1 f2)
  (lambda (value) (inverse-test f1 f2 value)))
; (test ((make-inverse-test square sqrt) 5.0) #f)
; (test ((make-inverse-test sqrt square) 5.0) #t)

; Problem e

; binary-inverse-test: function, function, number, number -> boolean
; to determine whether f1(f2(v1, v2), v2) is v1 when f1, f2, v1, v2 are given
; (binary-inverse-test * / 3 4) should produce #t
; (binary-inverse-test / * 3 4) should produce #t
(define (binary-inverse-test f1 f2 v1 v2)
  (= v1 (f1 (f2 v1 v2) v2)))
; (test (binary-inverse-test * / 3 4) #t)
; (test (binary-inverse-test / * 3 4) #t)

; Problem f

; make-binary-inverse-test: function, function -> function
; to return the binary-inverse-test of given two functions with the order
; (make-binary-inverse-test * /) should return the binary-inverse-test function with *, /
(define (make-binary-inverse-test f1 f2)
  (lambda (v1 v2) (binary-inverse-test f1 f2 v1 v2)))
; (test ((make-binary-inverse-test * /) 3 4) #t)