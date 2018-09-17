; Principles of Programming
; 20170504 이주안

#lang racket

; HW 05 Problem 01 - a

; pre-defined for prev
; variable for saving prev value
(define saved '*first-call*)

; prev: value -> value
; to return the last value unless it is the first call, otherwise, return *first-call*
; (prev 'a) -> *first-call*
; (prev 3) -> 'a
; (prev (+ 1 5)) -> 3
(define (prev val)
  (let ((return saved))
    (set! saved val)
    return))
; (test (prev 'a) '*first-call*)
; (test (prev 3) 'a)

; HW 05 Problem 01 - b

; make-prev: value -> (value -> value)
; to return the function prev same as above
; (make-prev '*first-call*) should produce same function as Problem 01 - a
(define (make-prev init)
  (lambda (val)
    (let ((return init))
      (set! init val)
      return)))
; (test ((make-prev 'wow) 'a) 'wow)

; HW 05 Problem 02

; make-account: integer, string -> (string, string -> integer)
; to work same as make-account function except this receives the password as input.
; user should give its own password as input of dispatch function
; (make-account 100 'secret-password) should produce dispatch funciton with password 'secret-password
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pw m)
    (if (eq? password pw)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (error "Incorrect password")))
  dispatch)
; (define acc (make-account 100 'secret-password))
; (test ((acc 'secret-password 'withdraw) 40) 60)
; ((acc 'some-other-password 'deposit) 50) should cause error

; HW 05 Problem 03

; Pre-defined variable for f
(define before -1)

; f: {0, 1} -> {0, 1}
; this is a function such that (+ (f 0) (f 1)) is
; 0 if subexpressions are evaluated from left to right or 1 if evaluated from right to left.
; (+ (f 0) (f 1)) produces 0 so subexpressions are evaluated from left to right
(define (f n)
  (cond ((eq? before -1)
         (set! before n)
         0)
        ((eq? before 0) 0)
        (else 1)))
; (test (+ (f 0) (f 1)) 0)
