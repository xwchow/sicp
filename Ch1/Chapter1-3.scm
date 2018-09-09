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

  

  
