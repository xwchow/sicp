(define (install-integer-package)
  ;; internal
  (define (integer->rational x)
    (make-rat x 1))
  ;; interface
  (define (tag x)
    (attach-tag 'integer x))
  (put 'make-int 'integer
       (lambda (x) (tag x)))
  (put 'raise 'integer
     (lambda (x)
       (integer->rational x)))
  'done)

(install-integer-package)

(define (make-int x)
  ((get 'make-int 'integer) x))

