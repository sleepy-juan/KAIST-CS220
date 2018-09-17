; Principles of Programming
; HW 02 Problem 05
; 20170504 이주안

#lang racket

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