;; Chapter 1.3:  Formulating Abstractions with Higher-Order Procedures
;; Exercise 1.29
(add-to-load-path ".")
(use-modules (util))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (next x) (+ x (* 2 h)))
  (* (/ h 3)
     (+ (f a)
        (f b)
        (* 4 (sum f (+ a h) next b))
        (* 2 (sum f (+ a h h) next b)))))

(simpsons-rule cube 0 1. 100)  ;; => 0.2500000000000004
(simpsons-rule cube 0 1. 1000) ;; => 0.25000000000000083

;; Exercise 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; Exercise 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (product identity 1 1+ n))

(factorial 10)

(define (pi-approx n)
  (define (term x)
    (define a
      (if (even? x)
          (+ x 2)
          (+ x 1)))
    (define b
      (if (even? x)
          (+ x 1)
          (+ x 2)))
    (/ a b))
  (* 4 (product term 1. 1+ n)))

(print (pi-approx 1000.))

;; Exercise 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a)
                                 result))))
  (iter a null-value))
    
(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;; Exercise 1.33
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner (if (filter a)
                            (term a)
                            null-value)
                        result))))
  (iter a null-value))

(define (sum-prime-squares a b)
  (filtered-accumulate prime? + 0 square a 1+ b))

(print (sum-prime-squares 1 10))

(define (product-coprimes n)
  (define (coprime x)
    (= (gcd x n) 1))
  (filtered-accumulate coprime * 1 identity 1 1+ (1- n)))

(print (product-coprimes 10))

;; Exercise 1.34
(define (f g)
  (g 2))

;; (2 2) is a type error

;; Exercise 1.35
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

;; Exercise 1.36
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (print guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
  
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0) ;; 34 iterations
(fixed-point (lambda (x) (* 0.5 (+ x (/ (log 1000) (log x))))) 2.0) ;; 10 iterations

;; Exercise 1.37
(define (cont-frac n d k)
  (define (rec i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i)
                    (rec (1+ i))))))
  (rec 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)

;; have to generate in reverse 
(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (1- i) (/ (n i)
                        (+ (d i)
                           result)))))
  (iter k 0))

;; Exercise 1.38
(+ 2 (cont-frac (lambda (i) 1.0)
                (lambda (i) (if (= 2 (remainder i 3))
                                (* (+ i 1) (/ 2 3))
                                1.0))
                20))

;; Exercise 1.39
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                             x
                             (- (square x))))
             (lambda (i) (1- (* 2 i)))
             k))

(tan-cf 0.5 20)
(tan-cf 0.75 20)
