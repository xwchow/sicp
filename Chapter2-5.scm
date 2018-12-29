#!/usr/bin/guile
!#

(add-to-load-path ".")
(use-modules (util))

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
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: 
              TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: 
              CONTENTS" datum)))

;; rational package
(define (install-rational-package)
  ;; internal procedures
  (define (numer r) (car r))
  (define (denom r) (cdr r))
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
  'done)

(define (make-rat a b)
  ((get 'make-rat 'rational) a b))

;; rectangular package
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

;; polar package
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
  (put 'add '(complex complex)
       (lambda (z1 z2) 
         (tag (add-complex z1 z2))))
  ;; For purposes of testing 2.82
  (put 'add-three '(complex complex complex)
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

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; Scheme number package
(define (install-scheme-number-package)
  ;; interface
  (put 'equ? '(scheme-number scheme-number)
     (lambda (x y)
       (= x y)))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0))))

;; Generic selectors.
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types: 
             APPLY-GENERIC"
            (list op type-tags))))))

(define (add a b)
  (apply-generic 'add a b))
;; For purposes of testing 2.82
(define (add-three a b c)
  (apply-generic 'add-three a b c))
(define (real-part z)
  (apply-generic 'real-part z))
(define (imag-part z) 
  (apply-generic 'imag-part z))
(define (magnitude z) 
  (apply-generic 'magnitude z))
(define (angle z) 
  (apply-generic 'angle z))

(define (install-packages)
  (install-rectangular-package)
  (install-polar-package)
  (install-complex-package)
  (install-scheme-number-package)
  (install-rational-package))
(install-packages)

;; Exercise 2.77
;; Chain of calls:
;; (magnitude z)
;; (apply-generic 'magnitude 'complex) 
;; (magnitude (contents z)) ;; within the complex library
;; (apply-generic 'magnitude 'rectangular)
;; (magnitude (contents (contents z))) ;; within the rectangular library
(define z (make-complex-from-real-imag 3 4))
(real-part z)

;; Exercise 2.78
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum: 
              TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum: 
              CONTENTS" datum))))

;; Exercise 2.79
;; Code below is included in the packages above.
(define (equ? a b)
  (apply-generic 'equ? a b))

;; scheme-number
;; (put 'equ? '(scheme-number scheme-number)
;;      (lambda (x y)
;;        (= x y)))
(equ? 3 3)
(equ? 3 4)

;; complex
;; (put 'equ? '(complex complex)
;;      (lambda (x y)
;;        (and (= (real-part x) (real-part y))
;;             (= (imag-part x) (imag-part y)))))
(equ? (make-complex-from-real-imag 3 4)
      (make-complex-from-real-imag 3 4))
(equ? (make-complex-from-real-imag 3 4)
      (make-complex-from-real-imag 3 5))

;; rational
;; (put 'equ? '(rational rational)
;;      (lambda (x y)
;;        (= (* (numer x) (denom y))
;;           (* (denom x) (numer y)))))
(equ? (make-rat 2 1) (make-rat 4 2))
(equ? (make-rat 2 1) (make-rat 3 2))

;; exercise 2.80
;; Code below is included in the packages above.
(define (=zero? x)
  (apply-generic '=zero? x))

;; scheme-number
;; (put '=zero? '(scheme-number)
;;      (lambda (x)
;;        (= x 0)))
(=zero? 1)
(=zero? 0)

;; complex
;; (put 'equ? '(complex)
;;      (lambda (x)
;;        (= (magnitude x) 0)))
(=zero? (make-complex-from-mag-ang 0 3))
(=zero? (make-complex-from-mag-ang 1 3))

;; rational
;; (put 'equ? '(rational)
;;      (lambda (x)
;;        (= (numer x) 0)))
(=zero? (make-rat 3 4))
(=zero? (make-rat 0 4))

;; Exercise 2.81
;; The program infinite loops; endlessly coercing complex to complex
;; and trying apply-generic again.
;; apply-generic does work correctly, although there is some wasted effort
;; to try coercion.
(define (scheme-number->complex n)
  (make-complex-from-real-imag 
   (contents n) 0))
(put-coercion 'scheme-number 'complex 
              scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "No method for these types"
                           (list op type-tags))
                    (let ((t1->t2 
                           (get-coercion type1
                                         type2))
                          (t2->t1 
                           (get-coercion type2 
                                         type1)))
                      (cond (t1->t2
                             (apply-generic 
                              op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic 
                              op a1 (t2->t1 a2)))
                            (else
                             (error 
                              "No method for 
                           these types"
                              (list 
                               op 
                               type-tags)))))))
              (error 
               "No method for these types"
               (list op type-tags)))))))

;; Test
(add 3 (make-complex-from-real-imag 3 4)) ;;  (complex rectangular 6 . 4)

;; Exercise 2.82
;; Coercion to the same type would not work in the case of an operation
;; being supported only on multi-types and not the same type.
;; If we can assume that there the type hierarchy does not permit multiple
;; inheritance, we can raise each type independently.
;; However, since we have not implemented raise, we will just implement
;; the naive strategy of coercion to the same type.
(define (error-out op type-tags)
  (error "No method for these types"
         (list op type-tags)))

(define (make-list x n)
  (if (= n 0)
      '()
      (cons x (make-list x (dec n)))))

(define (apply-generic op . args)
  (define (coerce-args choice args)
    ;; Returns args that have been coerced to choice type if possible
    ;; or #false otherwise.
    (if (null? args)
        '()
        (let ((min-arg (car args))
              (rest-args (coerce-args choice (cdr args))))
          (if (eq? choice (type-tag min-arg))
              (if rest-args (cons min-arg rest-args) #f)
              (let ((coerce-f (get-coercion (type-tag min-arg) choice)))
                (if (and coerce-f rest-args)
                    (cons (coerce-f min-arg) rest-args)
                    #f))))))

  (define (helper tags choices args)
    (if (null? choices)
        (error-out op tags)
        (let ((choice (car choices)))
          (let ((new-args (coerce-args choice args)))
            (if new-args
                (let ((proc (get op (map type-tag new-args))))
                  (if proc
                      (apply proc (map contents new-args))
                      (helper tags (cdr choices) args)))
                (helper tags (cdr choices) args))))))
  
  (let ((type-tags (map type-tag args)))
    (helper type-tags type-tags args)))

;; Test
(add-three 3 4 (make-complex-from-real-imag 3 4)) ;; (complex rectangular 10 . 4)

;; Exercise 2.83
;; Restoring these to ignore number? optimisation given that we now
;; have 2 number types.
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: 
              TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: 
              CONTENTS" datum)))

(define (raise x)
  (if (eq? (type-tag x) 'complex)
      x
      ((get 'raise (type-tag x)) (contents x))))

;; real
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

;; Test
(raise (make-real 3.2)) ;;  (complex rectangular 3.2 . 0)

;; rational
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

;; Test
(raise (make-rat 3 4)) ;; (real . 3/4)

;; integer
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

;; Test
(raise (make-int 3)) ;; (rational 3 . 1)

;; Exercise 2.84
;; The algorithm implemented here recursively raises the lowest type
;; in the hierarchy. If there is a tie for lowest, it raises the
;; an arbitrary argument of lowest type. This also solves the problem
;; of arguments needing to be converted to a common superclass.
(define (can-be-raised? a)
  (not (equal? a (raise a))))

(can-be-raised? (make-int 3)) ;; #t
(can-be-raised? (make-complex-from-real-imag 3 4));; #f

(define (compare-types a b)
  ;; Returns true if a can be raised into the same type as b.
  (cond ((eq? (type-tag a) (type-tag b)) #t)
        ((not (can-be-raised? a)) #f)
        (else (compare-types (raise a) b))))

(compare-types (make-int 3) (make-complex-from-real-imag 3 4)) ;; #t
(compare-types (make-int 3) (make-rat 3 4)) ;; #t
(compare-types (make-rat 3 4) (make-int 3)) ;; #f

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

(get-min-arg (list (make-int 3) (make-rat 3 4) (make-complex-from-real-imag 3 4)))
(get-min-arg (list (make-rat 3 4) (make-int 3) (make-complex-from-real-imag 3 4)))

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
    (if (can-be-raised? min-arg)
        (raise-arg args min-arg)
        #f)))

(raise-min-arg
 (raise-min-arg
  (raise-min-arg
   (raise-min-arg
    (raise-min-arg
     (raise-min-arg
      (list
       (make-int 3)
       (make-rat 3 4)
       (make-complex-from-real-imag 3 4))))))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((new-args (raise-min-arg args)))
            (if new-args
                (apply apply-generic op new-args)
                (error-out op type-tags)))))))

;; Test
(add-three
 (make-int 1) (make-rat 3 4) (make-real 2.5)) ;; (complex rectangular 4.25 . 0)

;; Exercise 2.85
(define (project arg)
  ;; Returns the projected arg if there is a projection.
  ;; False otherwise.
  (if (pair? arg)
      (let ((proj (get 'project (type-tag arg))))
        (if proj
            (proj (contents arg))
            #f))
      #f))

(real-part (make-complex-from-real-imag 3 4))
(project (make-complex-from-real-imag 3 4))
(project (make-real 3.5))
(project (make-rat 3 4))

(define (can-project? x)
  (if (project x)
      (equal? x (raise (project x)))
      #f))

(can-project? (make-complex-from-real-imag 3 4));; #f
(can-project? (make-complex-from-real-imag 3 0));; #t
(can-project? (make-real 3.5)) ;; #f
(can-project? (make-real 3.0)) ;; #t
(can-project? (make-rat 1 2)) ;; #f
(can-project? (make-rat 1 1)) ;; #t
(can-project? (make-int 1)) ;; #t

(define (drop x)
  (if (can-project? x)
      (drop (project x))
      x))

(drop (make-complex-from-real-imag 1.5 0)) ;; (real . 1.5)
(drop (make-complex-from-real-imag 1 0)) ;;  (integer . 1)
(drop (make-complex-from-real-imag 2 3)) ;;  (complex rectangular 2 . 3)

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
  (drop (apply helper op args)))

 ;; Exercise 2.86
;; Not implementing this here but this should not be difficult.
;; We just need to install the generic operations for sine/cosine/square etc
;; and implement them in each package which would be used to make
;; up a complex number.
