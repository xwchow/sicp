#!/usr/bin/guile
!#

(add-to-load-path ".")
(use-modules (util))
(load "arithmetic-package.scm")
(define (variable? e) (symbol? e))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (list '+ a1 a2))
(define (make-product a1 a2)
  (list '* a1 a2))
(define (sum? e)
  (and (pair? e) (eq? '+ (car e))))
(define (addend e)
  (cadr e))
(define (augend e)
  (caddr e))
(define (product? e)
  (and (pair? e) (eq? '* (car e))))
(define (multiplier e)
  (cadr e))
(define (multiplicand e)
  (caddr e))

;; ===============
;; | Polynomials | 
;; ==============
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? e) (symbol? e))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
            term-list
            (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) 
    (null? term-list))
  (define (make-term order coeff) 
    (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  ;; =======
  ;; | ADD |
  ;; =======
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) 
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 
                     (add-terms (rest-terms L1) 
                                L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 
                     (add-terms 
                      L1 
                      (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term 
                      (order t1)
                      (add (coeff t1) 
                           (coeff t2)))
                     (add-terms 
                      (rest-terms L1)
                      (rest-terms L2)))))))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (make-poly 
         (variable p1)
         (add-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: 
              ADD-POLY"
               (list p1 p2))))

  ;; =======
  ;; | MUL |
  ;; =======
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms 
         (mul-term-by-all-terms 
          (first-term L1) L2)
         (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term 
            (+ (order t1) (order t2))
            (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms 
            t1 
            (rest-terms L))))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (make-poly 
         (variable p1)
         (mul-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: 
              MUL-POLY"
               (list p1 p2))))

  ;; neg (Exercise 2.88)
  (define (neg-polynomial p)
    (define (neg-term term)
      (make-term (order term) (neg (coeff term))))
    (define (neg-terms terms)
      (if (empty-termlist? terms)
          (the-empty-termlist)
          (adjoin-term
           (neg-term (first-term terms))
           (neg-terms (rest-terms terms)))))
    (make-poly (variable p)
               (neg-terms (term-list p))))

  ;; =zero? (Exercise 2.87)
  (define (=zero-poly? p)
    (define (=zero-terms? terms)
      (if (empty-termlist? terms)
        #t
        (and (=zero? (coeff (first-term terms)))
             (=zero-terms? (rest-terms terms)))))
    (=zero-terms? (term-list p)))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) 
         (tag (mul-poly p1 p2))))
  (put 'the-empty-termlist 'term
       the-empty-termlist)
  (put 'make 'term
       make-term)
  (put 'adjoin 'term
       adjoin-term)
  (put 'make 'polynomial
       (lambda (var terms) 
         (tag (make-poly var terms))))
  (put '=zero? '(polynomial)
       (lambda (p)
         (=zero-poly? p)))
  (put 'neg '(polynomial)
       (lambda (p)
         (tag (neg-polynomial p))))
  'done)

(install-polynomial-package)
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
(define (dense-list->term-list term-list)
  (if (null? term-list)
      ((get 'the-empty-termlist 'term))
      ((get 'adjoin 'term)
       ((get 'make 'term) (dec (length term-list)) (car term-list))
       (dense-list->term-list (cdr term-list)))))

;; Exercise 2.87
;; Installed in the polynomial-package
;; (define (=zero-poly? p)
;;     (define (=zero-terms? terms)
;;       (if (empty-termlist? terms)
;;         #t
;;         (and (=zero? (coeff (first-term terms)))
;;              (=zero-terms? (rest-terms terms)))))
;;     (=zero-terms? (term-list p)))
;;
;; (put '=zero? '(polynomial)
;;      (lambda (p)
;;        (=zero-poly? p)))
(=zero? (make-polynomial 'x (list (list 1 0)))) ;; #t
(=zero? (make-polynomial 'x (list (list 1 2) (list 0 3)))) ;; #t

(define p1 (make-polynomial
            'x
            (dense-list->term-list '(2 3))))
(define p2 (make-polynomial
            'x
            (dense-list->term-list '(4 0 5))))
(define p3 (make-polynomial
            'y
            (dense-list->term-list (list p1 p2))))
;; (polynomial y (1 (polynomial x (1 2) (0 3))) (0 (polynomial x (2 4) (0 5))))

;; Exercise 2.88
(neg p1)
(define p3 (make-polynomial
            'y
            (dense-list->term-list (list (neg p1) (neg p2)))))


