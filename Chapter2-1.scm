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

