; Principles of Programming
; HW 01 Problem 03
; 20170504 이주안

#lang racket

; pascal-triangle: number -> string(triangular form of numbers)
; to make the pascal's triangle when the number of rows is given.
; (pascal-triangle 4) should produce like below:
; 1
; 1 1
; 1 2 1
; 1 3 3 1
(define (pascal-triangle n)
  ; pascal: number number -> number
  ; to calculate (row, col) position's element of pascal triangle.
  ; (pascal 3 2) should produce 2
  (define (pascal row col)
    (cond ((= row col) 1)
        ((= col 1) 1)
        (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))))
  ; (test (pascal 3 2) 2)

  ; pascal-display: number number -> string(some part of pascal's triangle), (n should be pre-defined before)
  ; to display (row, col) position's element of pascal triangle.
  ; (pascal-display 3 3) should display "1" if n is 3.
  (define (pascal-display row col)
    (display (pascal row col))
    (cond ((and (= row col) (not (= row n))) (display "\n") (pascal-display (+ row 1) 1))
          ((not (and (= row col) (= row n))) (display " ") (pascal-display row (+ col 1)))))
  ; Test cases are skipped in this case because the function not return but display the result.
  
  (pascal-display 1 1))
; test data:
; (pascal-triangle 4) =>
; 1
; 1 1
; 1 2 1
; 1 3 3 1
; (pascal-triangle 7) =>
; 1
; 1 1
; 1 2 1
; 1 3 3 1
; 1 4 6 4 1
; 1 5 10 10 5 1
; 1 6 15 20 15 6 1