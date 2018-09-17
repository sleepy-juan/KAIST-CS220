; Principles of Programming
; HW 01 Problem 01
; 20170504 이주안

#lang racket

; belows are assumed to be defined.

; double: number -> number
; to compute the double of given number
; (double 3) should produce 6
(define (double x) (* x 2))
; (test (double 3) 6)
; (test (double 2) 4)

; halve: number -> number
; to compute the half value of given number
; (halve 6) should produce 3
(define (halve x) (/ x 2))
; (test (halve 6) 3)
; (test (halve 12) 6)

; fast-mult: number number -> number
; to compute the multiplication of two given numbers.
; (fast-mult 10 10) should produce 100
(define (fast-mult a b)

  ; mult-iter: number number number -> number
  ; to compute the multiplication of a and b where a and b are second and third numbers by iterative method
  ; (mult-iter 0 10 10) should produce 100
  (define (mult-iter v a cnt)
    (if (= cnt 0)
        v
        (if (even? cnt)
            (mult-iter v (+ a a) (/ cnt 2))
            (mult-iter (+ v a) a (- cnt 1)))))
  ; (test (mult-iter 0 10 10) 100)
  
  (mult-iter 0 a b))
; (test (fast-mult 10 10) 100)
; (test (fast-mult 20 5) 100)
; (test (fast-mult 3 7) 21)