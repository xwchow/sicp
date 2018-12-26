o#!/usr/bin/guile
!#

(add-to-load-path ".")
(use-modules (util))

;; Exercise 2.73
;; 1. get is data-directed. It can retrieve rules such as the sum and product rules.
;; number? and variable? can technically be assimilated but what's the point?
;; 4. switch the order in the put calls
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) 
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ⟨more rules can be added here⟩
        (else (error "unknown expression type:
                      DERIV" exp))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var)
             1
             0))
        (else ((get 'deriv (operator exp))
               (operands exp)
               var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-sum-rule)
  (define (rule operands var)
    (make-sum (deriv (car operands) var)
              (deriv (cadr operands) var)))
  (put 'deriv '+ rule))

(define (install-product-rule)
  (define (rule operands var)
    (make-sum
     (make-product 
      (car operands)
      (deriv (cadr operands) var))
     (make-product 
      (deriv (car operands) var)
      (cadr operands))))
  (put 'deriv '* rule))

(define (install-exp-rule)
  (define (rule operands var)
    (make-product
     (make-product (exponent exp)
                   (make-exponentiation (car operands)
                                        (dec (cadr operands))))
     (deriv (car operands) var)))
  (put 'deriv '** rule))

;; Exercise 2.74a
;; Each division should be responsible for implement a get-record method
;; and register it in a (put 'get-record 'division).
;; THis method should return the employee if found and false if not found.
(define (get-record employee division)
  ((get 'get-record division) employee))

;; Exercise 2.74b
;; Each division should be responsible for implement a get-salary method
;; and register it in a (put 'get-salary 'division).
;; Recommendation:
;; A record can be specified as a list of pairs (key, value).
;; The list may be maintained in sorted order to improve performance.
;; Even better, we can use a balanced binary tree representation.
(define (get-salary employee-record division)
  ((get 'get-salary division) employee-record))

;; Exercise 2.74c
(define (find-employee-record employee divisions)
  (cond ((null? divisions) (error "Employee cannot be found" employee))
        (else (let ((record (get-record employee (car divisions))))
                (if (record)
                    record
                    (find-employee-record employee (cdr divisions)))))))

;; Exercise 2.74d
;; The new company should implement and put a get-record and get-salary

;; Exercise 2.75
;; Message parsing
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op: 
            MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* x (cos y)))
          ((eq? op 'imag-part) (* x (sin y)))
          ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          (else
           (error "Unknown op: 
            MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define datum (make-from-mag-ang 3 4))
(datum 'magnitude)
(datum 'real-part)

;; Exercise 2.76
;; Generic operations with explicit dispatch is the poorest fit.
;; Each generic operation will have to be updated every time a new type is
;; added.
;; Data-directed and message-passing are similar in that the changes are confined
;; to the new package added or the body of the new type.

