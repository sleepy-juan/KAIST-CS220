;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  This is the file hw4skel_2017.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define false #f)
(define nil '())

;;; GENERIC ARITHMETIC OPERATIONS

;;;   GN = ({number} X RepNum) U ({rational} X RepRat) U 
;;;        ({complex} X RepCom) U ({polynomial} X RepPoly)

;;;   (GN, GN) --> GN
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))


;;;   GN --> GN
(define (negate x) (apply-generic 'negate x))

;;;   GN --> Bool
(define (=zero? x) (apply-generic '=zero? x))


(define (equ? x y) (apply-generic 'equ? x y))

;;; a sample compound generic operation
;;;   GN --> GN
(define (square x) (mul x x))

;;; the ordinary  number package

(define (install-number-package)
  (define (tag x)
    (attach-tag 'number x))
  (define (make-number x) (tag x))
  (define (negate x) (tag (- x)))
  (define (zero? x) (= x 0))
  (define (add x y) (tag (+ x y)))
  (define (sub x y) (tag (- x y)))
  (define (mul x y) (tag (* x y)))
  (define (div x y) (tag (/ x y)))

  (define (=number? x y) (= x y)) ; =number?: RepNum x RepNum --> Sch-Boolean
  
  (put 'make 'number make-number)
  (put 'negate '(number) negate)
  (put '=zero? '(number) zero?)
  (put 'add '(number number) add)
  (put 'sub '(number number) sub)
  (put 'mul '(number number) mul)
  (put 'div '(number number) div)

  (put 'equ? '(number number) =number?)
  
  'done)

;;; Number Package User Interface

;;; A convenient external  procedure for building a generic
;;; ordinary number from Scheme numbers.

;;; Sch-Num --> ({number} X RepNum)
(define (create-number x) 
  ((get 'make 'number) x))

;;; the rational number package
(define (install-rational-package)
  (define (make-rat n d) (cons n d))
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (denom x) (numer y)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (denom x) (numer y)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))

  (define (negate-rat x) (tag (make-rat (negate (number x)) (denom x)))) ; RepRat --> Generic-Rational
  (define (=zero-rat? x) (=zero? (numer x))) ; RepRat --> Sch-Boolean
  (define (=rational? x y) (=zero-rat? (sub-rat x y))) ; RepRat x RetRat --> Sch-Boolean

  (define (repnum->reprat x) (make-rat (create-number x) (create-number 1))) ; RetNum --> RepRat

  ; I change the position since rep
  ;;;  ((RepRat,RepRat) --> T) --> ((RepNum,RepRat) --> T)
  (define (RRmethod->NRmethod method)
    (lambda (num rat)
      (method
       (repnum->reprat num)
       rat)))

  ; description of RRmethod->RNmethod is written in below.
  (define (RRmethod->RNmethod method)
    (lambda (rat num)
      (method
       rat
       (repnum->reprat num))))
  
  (define (tag x) (attach-tag 'rational x))
  (define (make-rational n d) (tag (make-rat n d)))
  (define (add-rational x y) (tag (add-rat x y)))
  (define (sub-rational x y) (tag (sub-rat x y)))
  (define (mul-rational x y) (tag (mul-rat x y)))
  (define (div-rational x y) (tag (div-rat x y)))
  (put 'make 'rational make-rational)
  (put 'add '(rational rational) add-rational)
  (put 'sub '(rational rational) sub-rational)
  (put 'mul '(rational rational) mul-rational)  
  (put 'div '(rational rational) div-rational)

  (put 'negate '(rational) negate-rat)
  (put '=zero? '(rational) =zero-rat?)
  (put 'equ? '(rational rational) =rational?)

  (put 'add '(rational number) (RRmethod->RNmethod add-rational))
  (put 'sub '(rational number) (RRmethod->RNmethod sub-rational))
  (put 'mul '(rational number) (RRmethod->RNmethod mul-rational))
  (put 'div '(rational number) (RRmethod->RNmethod div-rational))
  (put 'equ? '(rational number) (RRmethod->RNmethod =rational?))
  (put 'add '(number rational) (RRmethod->NRmethod add-rational))
  (put 'sub '(number rational) (RRmethod->NRmethod sub-rational))
  (put 'mul '(number rational) (RRmethod->NRmethod mul-rational))
  (put 'div '(number rational) (RRmethod->NRmethod div-rational))
  (put 'equ? '(number rational) (RRmethod->NRmethod =rational?))
  
  'done)


;;; Rational Package User Interface

;;; A convenient procedure for building a generic rational
;;; from generic numbers.

;;;    (GN, GN) --> ({rational} X RepRat)
(define  (create-rational n d)
  ((get 'make 'rational) n d))

;;; Complex Number Package, rectangular form a+bi

(define (install-complex-package)
  (define (make-com r i) (cons r i))
  (define (real x) (car x))
  (define (imag x) (cdr x))
  (define (add-com x y)
    (make-com (add (real x) (real y))
		  (add (imag x) (imag y))))
  (define (sub-com x y)
    (make-com (sub (real x) (real y))
		  (sub (imag x) (imag y))))
  (define (mul-com x y) 
    (make-com (sub (mul (real x) (real y)) 
		       (mul (imag x) (imag y)))
		  (add (mul (real x) (imag y))
		       (mul (real y) (imag x)))))
  (define (div-com x y)  
    (let ((com-conj (complex-conjugate y)))
       (let ((x-times-com-conj (mul-com x com-conj))
             (y-times-com-conj (mul-com y com-conj)))
	 (make-com (div (real x-times-com-conj) (real y-times-com-conj))
		   (div (imag x-times-com-conj) (real y-times-com-conj))))))

  ; For EXERCISE 13, description is on below
  (define (mydiv-com x y)
    (let ((com-conj (complex-conjugate y)))
      (let ((x-times-com-conj (mul-com x com-conj))
            (y-times-com-conj (mul-com y com-conj)))
        (make-com (create-rational (real x-times-com-conj) (real y-times-com-conj))
                  (create-rational (imag x-times-com-conj) (real y-times-com-conj))))))
  (define (complex-conjugate x)
    (make-com (real x) 
	      (negate (imag x))))
  (define (tag x) (attach-tag 'complex x))
  (define (make-complex n d) (tag (make-com n d)))
  (define (add-complex x y) (tag (add-com x y)))
  (define (sub-complex x y) (tag (sub-com x y)))
  (define (mul-complex x y) (tag (mul-com x y)))
  (define (div-complex x y) (tag (div-com x y)))
  (put 'make 'complex make-complex)
  (put 'add '(complex complex) add-complex)
  (put 'sub '(complex complex) sub-complex)
  (put 'mul '(complex complex) mul-complex)  
  ;(put 'div '(complex complex) div-complex)

  ; Modifying for EXERCISE 13
  (define (mydiv-complex x y) (tag (mydiv-com x y)))
  (put 'div '(complex complex) mydiv-complex)
  'done)


(define (create-complex r i)
  ((get 'make 'complex) r i))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;       This is the file type.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The bottom level typing system

(define attach-tag cons)

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad typed datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad typed datum -- CONTENTS" datum)))


;;; The apply-generic mechanism.  
;;;  Note that we don't deal with coercion here.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for the given types -- APPLY-GENERIC"
		 (list op type-tags))))))


;;; Code for creating the table, you don't need to worry about this.

(define (make-table)
  (let ((local-table (list '*table*)))

    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))


(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;
;ADD YOUR PROGRAM FROM HERE
;
; PUT YOUR COMMON ROUTINES HERE

(install-number-package)
(install-rational-package)
(install-complex-package)

; If you need to do, you may modify or add the funtions
; inside the packages above.
;
; DON'T FORGET YOUR COMMENTS LINES.

; EXERCISE 1 solution
; Don't forget your comments

; (a)
; procedure: Generic-Num --> Generic-Num

; (b)
; The mechanism for calculating the square of a number varies from the type of number. So, if we want to define square by apply-generic procedure, we should define all the cases of square.
; However, the procedure 'mul' is already defined for Generic-Num type, i.e. it is defined for general cases, so we don't have to define more than one funciton, (define (square x) (mul x x))

; EXERCISE 2 solution
; Don't forget your comments

; make-number: RepNum --> Generic-OrdNum
; negate: RepNum --> Generic-OrdNum
; zero?: RepNum --> Sch-Boolean

; EXERCISE 3 solution

; The common things of procedures keyed by lists of pairs of symbols is that they all use apply-generic procedure for defining them after installing packages.
; This is because they should handle for the general type, Generic-Num, so they take out the tags using 'map' for calculating all the combinations of types.

; However, the procedure, make-number need not to use apply-generic so it doesn't have to use the list of pairs of symbols.
; (Also, make-number receives RepNum type data which has no tag yet, so it indeed cannot use apply-generic.)
; This is not a problem because the table in tag-type system handles it just by 'equal?' method(not determining it is list or not).
; Additionaly, that's why we define create-number using 'get' directly.

; EXERCISE 4 - (b) Test

(define n2 (create-number 2))
(define n4 (create-number 4))
(define n6 (create-number 6))

(equ? n4 (sub n6 n2)) ; #t

; EXERCISE 5 - (a)

; second-try is a right way.
(define second-try (create-rational (create-number 9) (create-number 10)))
; When two Generic-Rat type data are added, they use mul procedure which is defined to use for Generic-Num, i.e. for some data with tags.
; However, first-try makes RepRat type data which has no tag, so we cannot use mul procedure.
; Therefore, first-try will cause error when adding two first-try-case data.

; EXERCISE 6 solution

; First, the procedure 'add' is already predefined above(more top-level than add-rational).
; Second, in the rational package, 'add' procedure is used. So, if it defines as 'add' again, there will be some crush between them.

; EXERCISE 7 - (b) Test

(define r1/1 (create-rational (create-number 1) (create-number 1)))
(define r1/2 (create-rational (create-number 1) (create-number 2)))
(define r1/3 (create-rational (create-number 1) (create-number 3)))

(equ? (sub r1/1 (mul r1/2 r1/3)) (add r1/2 r1/3))

; EXERCISE 9 solution

; RRmethod->RNmethod: ((RepRat x RetRat --> RepRat) --> (RepRat x RepNum --> RepRat))
; to produce a procedure that can calculate with rational and number paramters from that with rational and rational paramters.
; (RRmethod->RNmethod add) should produce a procedure that adds rational and number.
;(define (RRmethod->RNmethod method)
;  (lambda (rat num)
;    (method
;     rat
;     (repnum->reprat num))))
; (test ((RRmethod->RNmethod add) (create-rational (create-number 1) (create-number 3)) (create-number 1)) (create-rational (create-number 4) (create-number 3)))

; EXERCISE 10 - (b) Test

(define n3 (create-number 3))
(define r3 (create-rational (create-number 3) (create-number 1)))
(define r2/7 (create-rational (create-number 2) (create-number 7)))

(equ? n3 r3)
(equ? (sub (add n3 r2/7) r2/7) n3)

; EXERCISE 11

(define c1+3i (create-complex (create-number 1) (create-number 3)))
(define c5+0i (create-complex (create-number 5) (create-number 0)))

(div c1+3i c5+0i)

(define r1+3i/5 (create-rational c1+3i (create-number 5)))
; The value is same.

; EXERCISE 12

(define c1+2i (create-complex (create-number 1) (create-number 2)))
(div c1+3i c1+2i)
; it will return (complex (number . 1.4) number . 0.2)
; div-com procedure does this.

; EXERCISE 13

; mydiv-com: Generic-Complex x Generic-Complex --> Generic-Complex
; to calculate the division of two complex numbers.
; (div c1+3i c1+2i) should produce (complex (rational (number . 7) number . 5) rational (number . 1) number . 5)
;(define (mydiv-com x y)
;  (let ((com-conj (complex-conjugate y)))
;    (let ((x-times-com-conj (mul-com x com-conj))
;          (y-times-com-conj (mul-com y com-conj)))
;      (make-com (create-rational (real x-times-com-conj) (real y-times-com-conj))
;                (create-rational (imag x-times-com-conj) (real y-times-com-conj))))))
; (test (div c1+3i c1+2i) (create-complex (create-rational (create-number 7) (create-number 5)) (create-rational (create-number 1) (create-number 5))))

; Disadvantages are that it represents the numbers as rationals and the rational cannot be reduced form.
; Advantage is that there was the very simple modification in the code.

; EXERCISE 14

; install-mypackage: Void --> Void
; to install my packages that determine the equality of complex and rational
; (install-mypackage) should install belows
(define (install-mypackage)
  (define (real c) (car c))
  (define (imag c) (cdr c))

  ; com=com?: Generic-complex x Generic-complex --> Sch-Boolean
  ; to determine two inputs are same
  ; (com=com? c1+2i c1+3i) should produce #f
  (define (com=com? a b)
    (and (equ? (real a) (real b)) (equ? (imag a) (imag b))))
  ; (test (com=com? c1+2i c1+3i) #f)

  (put 'equ? '(complex complex) com=com?)
  
  (define (numer r) (car r))
  (define (denom r) (cdr r))

  ; =number?: Something --> Sch-Boolean
  ; to determine input is Generic-number or not
  ; (=number? (create-number 1)) should produce #t
  (define (=number? n) (equal? (car n) 'number))
  ; (test (=number? (create-number 1)) #t)

  ; parse-complex: (Generic-Num or Generic-Com) --> Generic-Com
  ; to change the form into Generic-Com
  ; (parse-complex (create-number 1)) should produce (complex (number . 1) number . 0)
  (define (parse-complex n)
    (if (=number? n)
        (create-complex n (create-number 0))
        n))
  ; (test (parse-complex (create-number 1)) (create-complex (create-number 1) (create-number 0)))

  ; rat=com?: Generic-Rat x Generic-Com --> Sch-Boolean
  ; to deterime two inputs are same
  ; (rat=com? (create-rational (create-number 1) (create-number 0)) (create-complex (create-number 1) (create-number 0))) should produce #t
  (define (rat=com? rat com)
    (equ? (div (parse-complex (numer rat)) (parse-complex (denom rat))) (attach-tag 'complex com)))
  ; (test (rat=com? (create-rational (create-number 1) (create-number 0)) (create-complex (create-number 1) (create-number 0))) #t)

  ; com=rat?: Generic-Com x Generic-Rat --> Sch-Boolean
  ; to deterime two inputs are same
  ; (com=rat? (create-complex (create-number 1) (create-number 0)) (create-rational (create-number 1) (create-number 0))) should produce #t
  (define (com=rat? com rat)
    (rat=com? rat com))
  ; (test (com=rat? (create-complex (create-number 1) (create-number 0)) (create-rational (create-number 1) (create-number 0))) #t)

  (put 'equ? '(rational complex) rat=com?)
  (put 'equ? '(complex rational) com=rat?)

  'done)

(install-mypackage)

(define c3+2i (create-complex (create-number 3) (create-number 2)))
(define r3+2i/5 (create-rational c3+2i (create-number 5)))
(define r3/5 (create-rational (create-number 3) (create-number 5)))
(define r2/5 (create-rational (create-number 2) (create-number 5)))
(define c3/5+2/5i (create-complex r3/5 r2/5))

(equ? r3+2i/5 c3/5+2/5i)

; REFERENCE : MIT 6.001 -- Structure and Interpretation of Computer Programs, Fall Semester, 1998