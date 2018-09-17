; Principles of Programming
; HW 01 Problem 04
; 20170504 이주안

#lang racket

; fast-expt: number number -> number
; to compute b to the n
; (fast-expt 5 5) should produce 3125
(define (fast-expt b n)

  ; expt-iter: number number number -> number 
  ; to compute b to the n by the iterative method (b, n are second and third parameter, respectively)
  ; (expt-iter 1 5 5) should produce 3125. 
  (define (expt-iter v b cnt)
    (if (= cnt 0)
        v
        (if (even? cnt)
            (expt-iter v (* b b) (/ cnt 2))
            (expt-iter (* v b) b (- cnt 1)))))
  ; (test (expt-iter 1 5 5) 3125)
  
  (expt-iter 1 b n))
; (test (fast-expt 5 5) 3125)
; (test (fast-expt 3 3) 27)
; (test (fast-expt 4 2) 16)