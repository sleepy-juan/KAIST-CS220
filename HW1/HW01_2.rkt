; Principles of Programming
; HW 01 Problem 02
; 20170504 이주안

#lang racket

; A: number nubmer -> number
; to compute Ackermann's function
; (A 1 10) should produce 1024
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))
; (test (A 1 10) 1024)
; (test (A 2 4) 65536)

; Q. What are the values of the following expressions?
; (A 1 10) is 1024
; (A 2 4)  is 65536
; (A 3 3)  is 65536

; Q. Give concise mathematical definitions
; for the functions computed by the procedures f,g and h for positive integer vlaues of n.
; For example, (k n) computes 5*n^2

; f: number -> number
; to compute Ackermann's function when x is 0
; (f 10) should produce 20
(define (f n) (A 0 n)) ; 2n
; (test (f 10) 20)
; (test (f 20) 40)

; g: number -> number
; to compute Ackermann's function when x is 1
; (g 10) should produce 1024
(define (g n) (A 1 n)) ; 2^n
; (test (g 10) 1024)
; (test (g 5) 32)

; h: number -> number
; to compute Ackermann's function when x is 2
; (h 3) should produce 16
(define (h n) (A 2 n)) ; 2^(2^(2^(2^...))) n number of 2s including base
; (test (h 3) 16)
; (test (h 4) 65536)

; k: number -> number
; to compute 5*n^2 when given number is n
; (k 2) should produce 20
(define (k n) (* 5 n n)) ; 5*n^2
; (test (k 2) 20)
; (test (k 3) 45)