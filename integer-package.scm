(define (install-integer-package)
  ;; internal
  (define (integer->rational x)
    (make-rat x 1))
  (define (equ? x y)
    (= x y))
  ;; interface
  (define (tag x)
    (attach-tag 'integer x))
  (put 'make-int 'integer
       (lambda (x) (tag x)))
  (put 'equ? '(integer integer)
       equ?)
  (put 'raise 'integer
     (lambda (x)
       (integer->rational x)))
  'done)

(install-integer-package)

