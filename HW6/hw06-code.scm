#lang racket
;;================================
; cs220 Spring 2017, hw6-code.scm
;;================================
;
(define nil '())

(define (write-line term)
  (begin (display term)
         (newline)))
;;; some basic stream operations

(define (stream-map proc stream)
  (if (stream-empty? stream)
      empty-stream
      (stream-cons (proc (stream-first stream))
                   (stream-map proc (stream-rest stream)))))

(define (add-streams s1 s2)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (stream-cons (+ (stream-first s1) (stream-first s2))
                      (add-streams (stream-rest s1)
                                   (stream-rest s2))))))

(define (scale-stream c stream)
  (stream-map (lambda (x) (* x c)) stream))

;;; power series operations

(define add-series add-streams)

(define scale-series scale-stream)

(define (negate-series s)
  (scale-series -1 s))

(define (subtract-series s1 s2)
  (add-series s1 (negate-series s2)))

;;; display the first n coefficients of a series

(define (show-series s nterms)
  (if (= nterms 0)
      'done
      (begin (write-line (stream-first s))
             (show-series (stream-rest s) (- nterms 1)))))

;;; return the coefficient of x^n

(define (series-coeff s n)  (stream-ref s n))

;;; create a (finite) series from a list of coefficients

(define (coeffs->series list-of-coeffs)
  (define zeros (stream-cons 0 zeros))
  (define (iter lst)
    (if (null? lst)
        zeros
        (stream-cons (car lst)
                     (iter (cdr lst)))))
  (iter list-of-coeffs))

;;; create a series from a procedure: nth term is P(n)
;;; requires non-neg-integers to be 0,1,2,3....
(define ones (stream-cons 1 ones))

;
; Problem 1 - must be copied to as a comment
; define non-neg-integers here
; needed for Problem 1 solution

; integers-from: number -> stream
; to produce the stream of integers from the given number
; (integers-from 1) should produce the stream of natural number
(define (integers-from n)
  (stream-cons n (integers-from (+ n 1))))
; (test (stream-ref (integers-from 1) 10) 10)

;(define non-neg-integers "you need to write program text to generate") 
(define non-neg-integers (integers-from 0))

(define (proc->series proc)
  (stream-map proc non-neg-integers))

; Exercise 1 : the definitions are copied from the class note.
(define (stream-enumerate-interval low high)
  (if (> low high)
      empty-stream
      (stream-cons
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-first s)
      (stream-ref (stream-rest s) (- n 1))))

(define (display-line x)
  (newline)
  (display x))

(define (show x)
  (display-line x)
  x)

; Exercise 1

(define x (stream-map show (stream-enumerate-interval 0 10)))
; (stream-ref x 5) displays 5
; (stream-ref x 7) displays 7

; Exercise 2

; defined in the hw
; the stream of 2^n
(define A (stream-cons 1 (scale-stream 2 A)))

; mul-streams: stream, stream -> stream
; to produce a stream which index is multiples of elements in a and that in b.
; (mul-streams (stream-enumerate-interval 0 3) (stream-enumerate-interval 0 3)) should produce stream of 0, 1, 4, 9
(define (mul-streams a b)
  (stream-cons
   (* (stream-first a) (stream-first b))
   (mul-streams (stream-rest a) (stream-rest b))))
; (test (stream-ref (mul-streams (stream-enumerate-interval 0 3) (stream-enumerate-interval 0 3)) 2) 4)

; define for stream B
; integers is the stream of non-negative integers starting from 1
; copied from class note and function integers-from is defined above.
(define integers (integers-from 1))

; define B
(define B (stream-cons 1 (mul-streams B integers)))

; answer for Exercise 2

; Stream A is the stream of 2^n, where n is an index.
; 'scale-stream' multiplies 2 to the A whose first value is 1, and 'stream-cons' appends it at the end of A.
; So A is produced like: (I mark checked index by scale-stream as ')
; 1
; 1' 1*2
; 1' 1*2' 1*2*2 ...

; Stream B is the stream of n!, where n is an index except 0! is defined as 1.
; 'mul-stream' multiplies two streams index-by-index, and B is produced by multiplication of integers and B itself.
; So the index of B is defined as multiple of previous index and element with previous index.
; B is produced like: (I mark checked index by mul-streams as ')
; 1
; 1' 1*1
; 1' 1*1' 1*1*2
; 1' 1*1' 1*1*2' 1*1*2*3 ...

; Exercise 3

; stream-pairs: stream -> stream
; to produce the all pairs of given stream
; (stream-pairs (stream-enumerate-inteveal 1 3)) should produce stream of all pairs
(define (stream-pairs s)
  (if (stream-empty? s)
      empty-stream
      (stream-append
       (stream-map
        (lambda (sn) (list (stream-first s) sn))
        (stream-rest s))
       (stream-pairs (stream-rest s)))))
; (stream-pairs (stream-enumerate-interval 1 3)) is the stream of 6 pairs. (test with show-series)

; (a) {(1 2) (1 3) (1 4) (1 5) (2 3) (2 4) (2 5) (3 4) (3 5) (4 5)}
; (b) basically, stream-pairs works with recursion.
;     at some step of recursion, it merges two streams (stream-map ...) and recursion result with (stream-rest s)
;     (stream-map ...) makes pairs that has (stream-fisrt s) as first index, and the elements in (stream-rest s) as second index.
;     By this process, stream-pairs make all the pairs of given stream.
; (c) {(1 2) (1 3) (1 4) (1 5) (1 6) ... }
;     I guess {(1 2) (1 3) (2 3) (1 4) (2 4) (3 4) (1 5) (2 5) ...} is better for infinite stream.

; modified-stream-pairs: stream -> stream
; to produce the all pairs of given stream
; (modified-stream-pairs integers) should produce stream of all pairs like: {(1 2), (1 3), (2 3), (1 4), (2 4), ...}
(define (modified-stream-pairs s)
  (define (next c)
    (if (= (- (cdr c) 1) (car c))
        (cons 0 (+ 1 (cdr c)))
        (cons (+ 1 (car c)) (cdr c))))
  (define (get c)
    (list (stream-ref s (car c)) (stream-ref s (cdr c))))
  (define (modified-stream-pairs-from c)
    (stream-cons (get c) (modified-stream-pairs-from (next c))))
  (modified-stream-pairs-from (cons 0 1)))
; (test (stream-ref (modified-stream-pairs integers) 100) (list 10 15))

; Problem 1

; non-neg-integers is defined above.
(define S1 ones)
(define S2 integers)

; Problem 2

; mul-series: stream, stream -> stream
; to produce the multiples of given two streams.
; (mul-series S1 S1) should produce S2
(define (mul-series s1 s2)
  (stream-cons (* (stream-first s1) (stream-first s2))
               (add-series (add-series (scale-stream (stream-first s1) (stream-rest s2))
                                       (scale-stream (stream-first s2) (stream-rest s1)))
                           (stream-cons 0 (mul-series (stream-rest s1) (stream-rest s2))))))
; (test (mul-series S1 S1) S2)

(define S2S2 (mul-series S2 S2))
; the coefficient of x^10 of S2 * S2 is 286

; Optional Question
; the coefficient of x^n is:
; (1/6)*(n^3) + n^2 + (11/6)*n + 1

; Test for Optional Question
; (define n 10)
; (test (+ (/ (* n n n) 6) (* n n) (/ (* 11 n) 6) 1) 286)

; Problem 3

; define 0 as stream
(define zeros (stream-cons 0 zeros))

; define 1 as stream
(define constant-one (stream-cons 1 zeros))

; invert-unit-series: stream -> stream
; to produce the inverse of given series
; (invert-unit-series S1) should produce the stream for 1-x
(define (invert-unit-series s)
  (define x (stream-cons 1 (negate-series (mul-series (stream-rest s) x))))
  x)
; (test (invert-unit-series S1) (stream-cons 1 (stream-cons -1 zeros)))
; (test (invert-unit-series S2) (steram-cons 1 (stream-cons -2 (stream-cons 1 zeros))))

; This doesn't fall into infinite loop since the x (variable in procedure) is defined as stream.
; At first, x is defined with stream-cons procedure, so it has 0 as first element and some other element which is not computed yet.
; This is why it is not inifinite loop even if it use x itself in the loop.

; Problem 4

; div-series: stream, stream -> stream
; to produce S1/S2 where S1 and S2 are given
; (div-series S1 S2) should produce (stream-cons 1 (stream-cons -1 zeros))
(define (div-series s1 s2)
  (if (= (stream-first s2) 0)
      (error "Divide by Zero")
      (mul-series s1 (invert-unit-series s2))))
; (test (div-series S1 S2) (stream-cons 1 (stream-cons -1 zeros)))
; (div-series S1 zeros) should produce Error

; Problem 5

; integrate-series-tail: stream -> stream
; to producte the integrated series of given series, s
; (integerate-series-tail S2) should produce x+x^2+x^3+...
(define (integrate-series-tail s)
  (define (ele-by-ele-divide s1 s2)
    (stream-cons (/ (stream-first s1) (stream-first s2))
                 (ele-by-ele-divide (stream-rest s1) (stream-rest s2))))
  (ele-by-ele-divide s integers))
; (test (integrate-series-tail S2) (stream-cons 0 ones))

; Problem 6

; defined in the problem
(define exp-series (stream-cons 1 (integrate-series-tail exp-series)))

; series for e^x is defined like:
; e^x = 1 + x/(1!) + x^2/(2!) + x^3/(3!) + ...
; Since the integrate-series-tail produces the integral as the way of dividing the index from the value of previous index,
; i.e. (a_n)*(x^n) -> (a_n)/(n+1)*(x^(n+1)),
; if the exp-series starts from 1 and produces next terms by recursively, it naturally generate the 1 / (factorial term) at each index.
; Thus, it will be the series for e^x

; define sin as the integration of cos
(define sin (stream-cons 0 (integrate-series-tail cos)))

; define cos as the integration of sin with the constant 1
(define cos (stream-cons 1 (negate-series (integrate-series-tail sin))))

; Problem 7

(define (integrate-series series constant-term)
  (stream-cons constant-term (integrate-series-tail series)))

;(define exp-series3 (integrate-series exp-series3 1))

; stream saves its value with delay function, and it keeps the rest of stream in the non-computed form.
; so, in common case, e.g. (define ones (stream-cons 1 ones)), it doesn't matter even if ones is not defined in (stream-cons 1 ones)
; However, for computing series recursively, the first element of stream must be defined and
; (define exp-series3 (integrate-series exp-series3 1)) do not define the very first element of the stream.
; So, it doen't work.

; Problem 8

; derivative-series: stream -> stream
; to produce the derivative of given series, s
; (derivative-series S2) should produce 2+6x+12x^2+20x^3+...
(define (derivative-series s)
  (define (dot-product s1 s2)
    (stream-cons (* (stream-first s1) (stream-first s2))
                 (dot-product (stream-rest s1) (stream-rest s2))))
  (dot-product (stream-rest s) integers))
; (test (derivative-series S1) S2)

; Problem 9

; tangent = sin / cos
(define tangent (div-series sin cos))
(display "tangent")
(show-series tangent 10)
(display "\n")

; secant = 1 / cos
(define secant (invert-unit-series cos))
(display "secant")
(show-series secant 10)
(display "\n")

; x*cot(x) = x / tan = 1 / (tan / x) (because first element of tan is 0 - Division By Zero)
(define x-cotangent-x (invert-unit-series (stream-rest tangent)))
(display "x-cotangent-x")
(show-series x-cotangent-x 10)
(display "\n")

; derivative of tangent
(define dtan (derivative-series tangent))
; secant^2
(define secant-square (mul-series secant secant))

; check whether they are same or not
(display "If this are all 0, they are same")
(show-series (subtract-series dtan secant-square) 10)