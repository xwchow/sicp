(define (install-rational-package)
  ;; internal procedures
  (define (numer r) (car r))
  (define (denom r) (cdr r))
  (define (rational->real r)
    (make-real (/ (numer r) (denom r))))
  (define (make-rat n d)
    (cons n d))
  (define (equ? r1 r2)
    (= (* (numer r1) (denom r2))
       (* (denom r1) (numer r2))))
  ;; interface
  (define (tag x)
    (attach-tag 'rational x))
  (put 'make-rat 'rational
       (lambda (a b)
         (tag (make-rat a b))))
  (put 'equ? '(rational rational)
       (lambda (x y)
         (equ? x y)))
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  (put 'raise 'rational
       (lambda (x)
         (rational->real x)))
  (put 'project 'rational
       (lambda (x)
         (make-int (round (/ (numer x)
                             (denom x))))))
  'done)

(install-rational-package)

(define (make-rat a b)
  ((get 'make-rat 'rational) a b))
