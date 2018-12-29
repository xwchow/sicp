(define (install-real-package)
  ;; internal
  (define (real->complex x)
    (make-complex-from-real-imag x 0))
  ;; interface
  (define (tag x)
    (attach-tag 'real x))
  (put 'make-real 'real
       (lambda (x) (tag x)))
  (put 'raise 'real
     (lambda (x)
       (real->complex x)))
  (put 'project 'real
       (lambda (x)
         (make-rat (round x) 1)))
  'done)

(install-real-package)

(define (make-real x)
  ((get 'make-real 'real) x))
