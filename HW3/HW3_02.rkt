#lang racket

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

; b

(define (total-weight mobile)
  (+
   (if (pair? (branch-structure (left-branch mobile)))
       (total-weight (branch-structure (left-branch mobile)))
       (branch-structure (left-branch mobile)))
   (if (pair? (branch-structure (right-branch mobile)))
       (total-weight (branch-structure (right-branch mobile)))
       (branch-structure (right-branch mobile)))))

; c

(define (torque branch)
  (if (pair? (branch-structure branch))
      (* (branch-length branch) (total-weight (branch-structure branch)))
      (* (branch-length branch) (branch-structure branch))))

(define (balanced mobile)
  (if (not (pair? mobile))
      #t
      (and
       (balanced (branch-structure (left-branch mobile)))
       (balanced (branch-structure (right-branch mobile)))
       (=
        (torque (left-branch mobile))
        (torque (right-branch mobile))))))

; d

; change cadr into cdr because pair made by cons function has no pointer in the second cell
; but the value, so we just pick the second value.