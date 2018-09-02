;; Exercise 1.2
;; Translate the following expression into prefix form:
(/ (+ 5 4 
      (- 2 3 (+ 6 (/ 4 5))))
   (* 3 (- 6 2) (- 2 7)))

;; Exercise 1.3
;; Define a procedure that takes three numbers as arguments
;; and returns the sum of the squares of the two larger numbers.
(define (square x) (* x x))

(define (sum-squares a b)
     (+ (square a) (square b)))

(define (proc a b c)
     (if (> a b)
         (if (> b c)
             (sum-squares a b)
             (sum-squares a c))
         (if (> a c)
             (sum-squares a b)
             (sum-squares b c))))

;; Exercise 1.7
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (good-enough? guess next)
    (< (abs (/ (- next guess) guess)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (define next (improve guess))
    (if (good-enough? guess next)
        guess
        (sqrt-iter next)))
  (sqrt-iter 1.0))

;; Exercise 1.8
(define (cube-root x)
  (define (good-enough? guess next)
    (< (abs (/ (- next guess) guess)) 0.001))
  (define (improve guess)
    (/ (+ (/ x (square guess))
          (* 2 guess))
       3))
  (define (cube-iter guess)
    (define next (improve guess))
    (if (good-enough? guess next)
        guess
        (cube-iter next)))
  (cube-iter 1.0))

;; Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10) ;; 1024 = 2^10
(A 2 4) ;; 65536 = 2^(2^(2^2))
(A 3 3) ;; 65536 = tower of twos with height (2^2)

;; Exercise 1.11
(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f-recursive (- n 1))
         (* 2 (f-recursive (- n 2)))
         (* 3 (f-recursive (- n 3))))))

(define (f-iter n)
  (define (f-inner a b c i)
    (if (= i n)
        c
        (f-inner b
                 c
                 (+ c (* 2 b) (* 3 a))
                 (+ i 1))))
  (if (< n 3)
      n
      (f-inner 0 1 2 2)))

;; Exercise 1.12
(define (pascal n k)
  (cond ((= n 0) 0)
        ((= k 0) 1)
        ((= k n) 1)
        (else (+ (pascal (- n 1) k)
                 (pascal (- n 1) (- k 1))))))

;; Exercise 1.15
(define (cube x) (* x x x))

(define (sine angle)
  (define (p x)
    (display "p") (newline)
    (- (* 3 x) (* 4 (cube x))))
  (if (< (abs angle) 0.1)
      angle
      (p (sine (/ angle 3)))))

(sine 12.15)

;; Exercise 1.16
;; Iterative version of fast-expt
(define (expt base exp)
  (define (square x) (* x x))
  (define (even? x) (= (remainder x 2) 0))
  (define (expt-iter a b n)
    (cond ((= n 0) a)
          ((even? n) (expt-iter a (square b) (/ n 2)))
          (else (expt-iter (* a b) b (- n 1)))))
  (expt-iter 1 base exp))
(expt 3 5)

;; Exercise 1.7
;; Fast multiplication
(define (mul a b)
  (define (even? x) (= (remainder x 2) 0))
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (mul (double a) (halve b)))
        (else (+ a (mul a (- b 1))))))

(mul 31 12)

;; Exercise 1.8
;; Fast multiplication with iteration
(define (mul-i x y)
  (define (even? x) (= (remainder x 2) 0))
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (define (mul-iter a b n)
    (cond ((= n 0) 0)
          ((= n 1) (+ a b))
          ((even? n) (mul-iter a (double b) (halve n)))
          (else (mul-iter (+ a b) b (- n 1)))))
  (mul-iter 0 x y))
 
(mul-i 31 13)

;; Exercise 1.19
;; Fib-iter
(define (fib n)
  (define (even? x) (= (remainder x 2) 0))
  (define (halve x) (/ x 2))
  (define (square x) (* x x))
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (square p) (square q))
                     (+ (* 2 p q) (square q))
                     (halve count)))
          (else
           (fib-iter (+ (* b q) (* a q) (* a p))
                     (+ (* b p) (* a q))
                     p
                     q
                     (- count 1)))))
  (fib-iter 1 0 0 1 n))

(fib 200)

