#!/usr/bin/guile
!#

(add-to-load-path ".")
(use-modules (util))

(define EPS 1e-8)

;; Let's define our own put and get functions here so we can test our code.
(define env (make-hash-table))
(define (put op type-tags f)
  (hash-set! env (cons op type-tags) f))
(define (get op type-tags)
  (hash-ref env (cons op type-tags)))

(define env-coercion (make-hash-table))
(define (put-coercion t1 t2 f)
  (hash-set! env-coercion (cons t1 t2) f))
(define (get-coercion t1 t2)
  (hash-ref env-coercion (cons t1 t2)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'real)
        (else (error "Bad tagged datum: 
              TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum: 
              CONTENTS" datum))))

;; Internal methods
(define (error-out op type-tags)
  (error "No method for these types"
         (list op type-tags)))

(define (raise arg)
  ;; Returns the raised arg if there is a projection.
  ;; False otherwise.
  (if (or (number? arg)
          (and (pair? arg) (symbol? (type-tag arg))))
      (let ((rs (get 'raise (type-tag arg))))
        (if rs
            (rs (contents arg))
            #f))
      #f))

(define (compare-types a b)
  ;; Returns true if a can be raised into the same type as b.
  (cond ((eq? (type-tag a) (type-tag b)) #t)
        ((not (raise a)) #f)
        (else (compare-types (raise a) b))))

(define (lower-arg a b)
  (if (compare-types a b)
      a
      b))

(define (get-min-arg args)
  (define (helper args min-arg)
    (if (null? args)
        min-arg
        (helper (cdr args) (lower-arg min-arg (car args)))))
  (helper (cdr args) (car args)))

(define (raise-min-arg args)
  ;; Returns a list of args with the arg with the lowest type raised once.
  ;; Returns false if the lowest type cannot be raised.
  (define (raise-arg args min-arg)
    (cond ((null? args) (error "min-arg not found in args"
                               (list min-arg args)))
          ((equal? (car args) min-arg)
           (cons (raise min-arg) (cdr args)))
          (else (cons (car args) (raise-arg (cdr args) min-arg)))))

  (let ((min-arg (get-min-arg args)))
    (if (raise min-arg)
        (raise-arg args min-arg)
        #f)))

(define (project arg)
  ;; Returns the projected arg if there is a projection.
  ;; False otherwise.
  (if (or (number? arg)
          (and (pair? arg) (symbol? (type-tag arg))))
      (let ((proj (get 'project (type-tag arg))))
        (if proj
            (proj (contents arg))
            #f))
      #f))

(define (can-project? x)
  (if (project x)
      (equal? x (raise (project x)))
      #f))

(define (drop x)
  (if (can-project? x)
      (drop (project x))
      x))

;; apply-generic which performs iterative raising and drops the final result.
;; This implementation omits dropping the result because it requires
;; that we implement the operators used in the complex-package
;; in the rational and integer packages.
(define (apply-generic op . args)
  (define (helper op . args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            (let ((new-args (raise-min-arg args)))
              (if new-args
                  (apply helper op new-args)
                  (error-out op type-tags)))))))
  (apply helper op args))

;; Generic selectors
(define (add a b)
  (apply-generic 'add a b))
(define (add-triple a b c)
  (apply-generic 'add-triple a b c))
(define (real-part z) 
  (apply-generic 'real-part z))
(define (imag-part z) 
  (apply-generic 'imag-part z))
(define (magnitude z) 
  (apply-generic 'magnitude z))
(define (angle z) 
  (apply-generic 'angle z))
(define (equ? a b)
  (apply-generic 'equ? a b))
(define (=zero? x)
  (apply-generic '=zero? x))

;; helper constructor methods
(define (make-int x)
  ((get 'make-int 'integer) x))
(define (make-rat a b)
  ((get 'make-rat 'rational) a b))
(define (make-real x)
  ((get 'make-real 'real) x))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (install-packages)
  (load "integer-package.scm")
  (load "complex-package.scm")
  (load "real-package.scm")
  (load "rational-package.scm"))
(install-packages)

;; ==============
;; | Test Suite |
;; ==============
(define (close-enough? a b)
  (< (abs (- a b)) EPS))

;; real
(if (equal?
     (raise (make-real 3.5))
     (make-complex-from-real-imag 3.5 0))
    #t
    (error "(raise (make-real 3.5))"))

(if (equal?
     (drop (make-real 3.0))
     (make-int 3.0))
    #t
    (error "(drop (make-real 3.0))"))

(if (close-enough? (add (make-real 3.2) (make-real 5.4)) 8.6)
      #t
      (error "(add (make-real 3.2) (make-real 5.4))"))

(if (close-enough?
     (add 3.2 5.4)
     8.6)
    #t
    (error "(add (make-real 3.2) (make-real 5.4))"))

;; complex
(if (close-enough?
     (drop (make-complex-from-real-imag 1.5 0))
     1.5)
    #t
    (error "(drop (make-complex-from-real-imag 1.5 0)"))

(if (equal?
     (drop (make-complex-from-real-imag 2 3))
     (make-complex-from-real-imag 2 3))
    #t
    (error "(drop (make-complex-from-real-imag 2 3))"))

;; apply-generic
(if (equal?
     (add-triple (make-rat 3 4) (make-int 4) (make-complex-from-real-imag 3 4))
     (make-complex-from-real-imag (/ 31 4) 4))
    #t
    (error "(add-triple (make-rat 3 4) (make-int 4) (make-complex-from-real-imag 3 4))"))

