;; rectangular
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) 
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) 
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

;; polar
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

;; complex
(define (install-complex-package)
  ;; imported procedures from rectangular 
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 
          'rectangular) 
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) 
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag 
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag 
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang 
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang 
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  (define (equ? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))

  (install-rectangular-package)
  (install-polar-package)

  (put 'add '(complex complex)
       (lambda (z1 z2) 
         (tag (add-complex z1 z2))))
  ;; For purposes of testing 2.82
  (put 'add-triple '(complex complex complex)
       (lambda (z1 z2 z3)
         (tag (add-complex z1 (add-complex z2 z3)))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) 
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) 
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) 
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex)
       (lambda (x y)
         (equ? x y)))
  (put '=zero? '(complex)
       (lambda (x) (= (magnitude x) 0)))
  (put 'project 'complex
       (lambda (x) (make-real (real-part x))))
  'done)

(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
