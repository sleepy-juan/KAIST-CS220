; Principles of Programming
; 20170504 이주안

#lang racket

; HW 03 Problem 01

; deep-reverse: list -> list
; to reverse the given list deeply i.e. reverse even the element if it is the list
; (deep-reverse (list 1 (list 2 3) 4)) should produce '(4 (3 2) 1)
(define (deep-reverse items)
  (if (null? items)
      empty
      (if (pair? (car items))
          (append (deep-reverse (cdr items)) (list (deep-reverse (car items))))
          (append (deep-reverse (cdr items)) (list (car items))))))
; (test (deep-reverse (list 1 2 3)) '(3 2 1))
; (test (deep-reverse (list 1 (list 2 3) 4)) '(4 (3 2) 1))

; HW 03 Problem 02

; belows are assumed to be defined in the problem

; make-mobile: list, list -> list
; to make a mobile with two branches
; (make-mobile <b1> <b2>) makes a mobile with branches <b1> and <b2>
(define (make-mobile left right)
  (list left right))
; I skipped the test cases for this(make-branch is not yet implemented and hard to represent)

; make-branch number, number(OR list) -> list
; to make a branch with length and structure,
; and structure can be number(weight) or list(other mobile)
; (make-branch 1 2) should produce '(1 2)
(define (make-branch length structure)
  (list length structure))
; (test (make-branch 1 2) '(1 2))
; I skipped test case for using another mobile as a structure

; Define some branches and mobiles for below descriptions
(define b1 (make-branch 2 1))
(define b2 (make-branch 2 1))
(define m1 (make-mobile b1 b2))
(define b3 (make-branch 3 m1))
(define b4 (make-branch 2 3))
(define m2 (make-mobile b3 b4))
(define m3 (make-mobile b1 b4))

; Problem a

; left-branch: list -> list
; to return the left branch of the mobile
; (left-branch m1) should return b1
(define (left-branch mobile) (car mobile))
; (test (left-branch m1) b1)
; (test (left-branch m2) b3)

; right-branch: list -> list
; to return the right branch of the mobile
; (right-branch m1) should return b2
(define (right-branch mobile) (cadr mobile))
; (test (right-branch m1) b2)
; (test (right-branch m2) b4)

; branch-length: list -> number
; to return the length of the branch
; (branch-length b1) should return 2
(define (branch-length branch) (car branch))
; (test (branch-length b1) 2)
; (test (branch-length b3) 3)

; branch-structure: list -> number (OR list)
; to return the structure(weight or mobile) of the branch
; (branch-structure b1) should return 1
(define (branch-structure branch) (cadr branch))
; (test (branch-structure b1) 1)
; (test (branch-structure b3) m1)

; Problem b

; total-weight: list(mobile) -> number
; to calculate the total weight of given mobile
; (total-weight m2) should produce 5
(define (total-weight mobile)
  (+
   (if (pair? (branch-structure (left-branch mobile)))
       (total-weight (branch-structure (left-branch mobile)))
       (branch-structure (left-branch mobile)))
   (if (pair? (branch-structure (right-branch mobile)))
       (total-weight (branch-structure (right-branch mobile)))
       (branch-structure (right-branch mobile)))))
; (test (total-weight m1) 2)
; (test (total-weight m2) 5)

; Problem c

; torque: list(branch) -> number
; to calculate the torque of given branch
; (torque b3) should produce 6
(define (torque branch)
  (if (pair? (branch-structure branch))
      (* (branch-length branch) (total-weight (branch-structure branch)))
      (* (branch-length branch) (branch-structure branch))))
; (test (torque b1) 2)
; (test (torque b3) 6)

; balanced: list(mobile) -> boolean
; to determine whether given mobile is balanced
; (balanced m2) should produce #t
(define (balanced mobile)
  (if (not (pair? mobile))
      #t
      (and
       (balanced (branch-structure (left-branch mobile)))
       (balanced (branch-structure (right-branch mobile)))
       (=
        (torque (left-branch mobile))
        (torque (right-branch mobile))))))
; (test (balanced m2) #t)
; (test (balanced m3) #f)

; Problem d

; CHANGE (define (right-branch mobile) (cadr mobile))
; INTO   (define (right-branch mobile) (cdr mobile))

; CHANGE (define (branch-structure branch) (cadr branch))
; INTO   (define (branch-structure branch) (cdr branch))

; because the last element of list is empty but that of pair is value

; HW 03 Problem 03

; accumulate: function, value, list -> value(it can be varied by given function)
; to compute the accumulation of items by the operator f with initial value init
; (accumulate + 0 (list 1 2 3 4 5)) should produce 15
(define (accumulate f init items)
  (if (null? items)
      init
      (f (car items) (accumulate f init (cdr items)))))
; (test (accumulate + 0 (list 1 2 3)) 6)

; horner-eval: number, list -> number
; to compute the value of polynomial with the method of horner's rule
; (horner-eval 3 (list 1 2 3)) should produce 34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))
; (test (horner-eval 3 (list 1 2 3)) 34)
; (test (horner-eval 2 (list 1 0 1)) 5)

; HW 03 Problem 04

; accumulate-n: function, value, list of lists with same size -> list
; to compute the accumulation of each element of items by the operator f with initial value
; (accumulate-n + 0 (list (list 1 2) (list 3 4))) should produce '(4 6)
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      empty
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
; (test (accumulate-n + 0 (list (list 1 2) (list 3 4))) '(4 6))

; HW 03 Problem 05

; fold-right: function, value, list -> value
; same function as accumulate
; (fold-right + 0 (list 1 2 3 4 5)) should produce 15
(define (fold-right f init items)
  (if (null? items)
      init
      (f (car items) (fold-right f init (cdr items)))))
; (test (fold-right + 0 (list 1 2 3)) 6)

; fold-left: function, value, list -> value
; to do same calculation as fold-right but in opposite direction
; (fold-left / 1 (list 1 2 3)) should produce 1/6
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest)) (cdr rest))))
    (iter initial sequence))
; (test (fold-left / 1 (list 1 2 3)) (/ 1 6))

; (fold-right / 1 (list 1 2 3)) produce 3/2
; (fold-left / 1 (list 1 2 3)) produce 1/6
; (fold-right list empty (list 1 2 3)) produce '(1 (2 (3 ())))
; (fold-left list empty (list 1 2 3)) produce '(((() 1) 2) 3)

; the condition of the operator is that:
; the commutative law should be satisfied for the operator

; HW 03 Problem 06

; equal?: value, value -> boolean
; to determine whether two given values are same
; (equal? '(a b c) '(a b c)) should produce #t
(define (equal? a b)
  (if (and (pair? a) (pair? b))
      (and
       (equal? (car a) (car b))
       (equal? (cdr a) (cdr b)))
      (eq? a b)))
; (test (equal? '(a b c) '(a b c)) #t)
; (test (equal? '(a (b c)) '(a (b c)) #t)
; (test (equal? '(a (b c)) '((a b) c) #f)