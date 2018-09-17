; Principles of Programming
; HW 01 Problem 01
; 20170504 이주안

#lang racket

; cube: number -> nuumber
; to compute the cube of some value
; (cube 3) should produce 27
(define (cube x) (* x x x))
; (test (cube 3) 27)
; (test (cube 2) 8)
; (test (cube 4) 64)

; cube-root: number -> number
; to compute the cube-root of some value using Newton's method
; (cube-root 27) should produce about 3
(define (cube-root x)
  ; good-enough?: number->boolean (x should be pre-defined before)
  ; to decide whether the given number is close enough to the answer
  ; (good-enough? 3) may produce #t if x is defined close enough to 3
  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) 0.000001))
  ; (define x 3.0001)
  ; (test (good-enough? 3) #t)

  ; improve: number -> number (x should be pre-defined before)
  ; to calculate the next appriximation based on given number using Newton's method
  ; (improve 3) may produce 2+2/27 if x is defined as 2
  (define (improve guess)
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
  ; (define x 2)
  ; (test (improve 3) (+ 2 (/ 2 27)))

  ; cube-iter: number -> number (good-enough? and improve should be pre-defined before)
  ; real body for calculating cube-root by iteration.
  ; (cube-iter 1.0) may produce cube-root of x
  (define (cube-iter guess)
    (if (good-enough? guess)
        guess
        (cube-iter (improve guess))))
  ; (define x 27)
  ; (test (cube-iter 1.0) 3)
  
  (cube-iter 1.0))
; (test (cube-root 27) 3)
; (test (cube-root 64) 4)
; (test (cube-root 8) 2)