#!/usr/bin/guile
!#

(add-to-load-path ".")
(use-modules (util))

;; Exercise 2.1
 (define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
        (cons (/ (- n) g) (/ (- d) g))
        (cons (/ n g) (/ d g)))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

;; Exercise 2.2
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
(define (midpoint-segment s)
  (let ((p1 (start-segment s))
        (p2 (end-segment s)))
    (make-point (average (x-point p1) (x-point p2))
                (average (y-point p1) (y-point p2)))))

(print-point
 (midpoint-segment
  (make-segment (make-point 4 6)
                (make-point 20 14))))

;; Exercise 2.3
(define (make-rectangle lower-left upper-right)
  (cons lower-left upper-right))
(define (upper-right r) (car r))
(define (lower-left r) (cdr r))
(define (get-width r)
  (- (x-point (upper-right r))
     (x-point (lower-left r))))
(define (get-height r)
  (- (y-point (upper-right r))
     (y-point (lower-left r))))

(define (make-rectangle lower-left upper-right)
  (let ((w (- (x-point upper-right)
              (x-point lower-left)))
        (h (- (y-point upper-right)
              (y-point lower-left))))
    (cons lower-left (cons w h))))
(define (lower-left r) (car r))
(define (get-width r) (car (cdr r)))
(define (get-height r) (cdr (cdr r)))

(define (perimeter r)
  (* 2 (+ (get-width r)
          (get-height r))))
(define (area r)
  (* (get-width r)
     (get-height r)))

;; Exercise 2.4
(define (my-cons x y)
  (lambda (m) (m x y)))
(define (my-car z)
  (z (lambda (x y) x)))
(define (my-cdr z)
  (z (lambda (x y) y)))

;; Exercise 2.5
(define (my-log n b)
  (if (divides? b n)
      (1+ (my-log (/ n b) b))
      0))
(define (make-ipair a b)
  (* (expt 2 a)
     (expt 3 b)))
(define (car-ipair p)
  (my-log p 2))
(define (cdr-ipair p)
  (my-log p 3))

;; Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

;; Exercise 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound ival) (cdr ival))
(define (lower-bound ival) (car ival))

;; Exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; Exercise 2.10
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (within ival x)
  (and (<= x (upper-bound ival))
       (>= x (lower-bound ival))))
(define (div-interval x y)
  (if (within y 0)
      (error "interval spans zero" y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define span (make-interval -1 1))
;; (div-interval (make-interval 3 5) span)

;; Exercise 2.11
(define (mul-interval-2 x y)
  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))
    (cond ((< (upper-bound x) 0)
           (cond ((< (upper-bound y) 0)
                  (make-interval (* x2 y2) (* x1 y1)))
                 ((> (lower-bound y) 0)
                  (make-interval (* x1 y2) (* x2 y1)))
                 (else
                  (make-interval (* x1 y2) (* x1 y1)))))
          ((> (lower-bound x) 0)
           (cond ((< (upper-bound y) 0)
                  (make-interval (* x2 y1) (* x1 y2)))
                 ((> (lower-bound y) 0)
                  (make-interval (* x1 y1) (* x2 y2)))
                 (else
                  (make-interval (* x2 y1) (* x2 y2)))))
          (else
           (cond ((< (upper-bound y) 0)
                  (make-interval (* x2 y1) (* x1 y1)))
                 ((> (lower-bound y) 0)
                  (make-interval (* x1 y2) (* x2 y2)))
                 (else
                  (make-interval (min (* x1 y2) (* x2 y1))
                                 (max (* x1 y2) (* x2 y2)))))))))

(define neg-span (make-interval -3 -2))
(define pos-span (make-interval 5 7))
(define mid-span (make-interval -11 13))

(define (test-mul-interval x y)
  (define (eq a b)
    (and (= (lower-bound a) (lower-bound b))
         (= (upper-bound a) (upper-bound b))))
  (let ((exp (mul-interval x y))
        (ans (mul-interval-2 x y)))
    (if (eq exp ans)
        #t
        (error "Failed test. Expected = " exp " Answer = " ans))))

(test-mul-interval neg-span neg-span)
(test-mul-interval neg-span mid-span)
(test-mul-interval neg-span pos-span)
(test-mul-interval mid-span neg-span)
(test-mul-interval mid-span mid-span)
(test-mul-interval mid-span pos-span)
(test-mul-interval pos-span neg-span)
(test-mul-interval pos-span mid-span)
(test-mul-interval pos-span pos-span)

;; Exercise 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))

(define (width i)
  (/ (- (upper-bound i) 
        (lower-bound i)) 
     2))

(define (make-center-percent center percent)
  (let ((width (* (/ percent 100) center)))
    (make-interval (- center width)
                   (+ center width))))

(define (percent ival)
  (* 100 (/ (width ival)
            (center ival))))

(percent (make-center-percent 10 10))

;; Exercise 2.13
;; Let the two intervals be (x1 - p1x1, x1 + p1x1) and (x2 - p1x2, x2 + p2x2)
;; Product gives us (x1x2 - p2x1x2 - p1x1x2 + p1p2x1x2, ...)
;; Assume p1p2x1x2 is negigible
;; (x1x2 - (p1 + p2)x1x2, ...)
;; tolerances are additive

;; Exercise 2.14
(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (par1 r1 r2)
  (div-interval
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1 )))
    (div-interval
     one
     (add-interval
      (div-interval one r1)
      (div-interval one r2)))))

(define A (make-center-percent 10 0.01))
(define B (make-center-percent 50 5))

(let ((ival (par1 A A)))
  (format #f "(~f, ~f)" (center ival) (percent ival)))

(let ((ival (par2 A A)))
  (format #f "(~f, ~f)" (center ival) (percent ival)))

(let ((ival (par1 B B)))
  (format #f "(~f, ~f)" (center ival) (percent ival)))

(let ((ival (par2 B B)))
  (format #f "(~f, ~f)" (center ival) (percent ival)))

;; Exercise 2.15
;; Appears to be true that par2 is better. 
;; Why? Uncertainty increases as more operations are performed with uncertain intervals.
;; Imagine repeated multiplications with (R1/R2) * (R2/R1).
;; Program cannot recognize that a term has been used.

;; Exercise 2.16
;; Distributivity does not hold. Associativity does not hold.
