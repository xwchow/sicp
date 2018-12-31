;; real numbers are represented as scheme numbers directly
;; without the 'real tag
(define (install-real-package)
  ;; internal
  (define (real->complex x)
    (make-complex-from-real-imag x 0))
  (define (add x y)
    (+ x y))
  (define (mul x y)
    (* x y))
  (define (div x y)
     (/ x y))
  (define (neg x)
    (- x))
  (define (gcd x y)
    (if (= y 0)
        x
        (gcd y (remainder x y))))
  ;; interface
  (put 'add '(real real)
       add)
  (put 'mul '(real real)
       mul)
  (put 'div '(real real)
       div)
  (put 'make-real 'real
       (lambda (x) x))
  (put 'raise 'real
     (lambda (x)
       (real->complex x)))
  (put 'project 'real
       (lambda (x)
         (make-rat (round x) 1)))
  (put 'neg '(real)
       neg)
  'done)

(install-real-package)
