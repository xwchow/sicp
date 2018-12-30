;; real numbers are represented as scheme numbers directly
;; without the 'real tag
(define (install-real-package)
  ;; internal
  (define (real->complex x)
    (make-complex-from-real-imag x 0))
  (define (add x y)
    (+ x y))
  ;; interface
  (put 'add '(real real)
       (lambda (x y)
         (add x y)))
  (put 'make-real 'real
       (lambda (x) x))
  (put 'raise 'real
     (lambda (x)
       (real->complex x)))
  (put 'project 'real
       (lambda (x)
         (make-rat (round x) 1)))
  'done)

(install-real-package)
