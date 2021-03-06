#!/usr/bin/guile
!#

(add-to-load-path ".")
(use-modules (util))
(use-modules (arithmetic-package))

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
;; | Term Lists |
;; ==============
(define (install-dense-package)
    ;; representation of term lists
  (define (adjoin-term term term-list)
    ;; Here we make the assumption that the order of term is
    ;; strictly larger than terms in the term-list.
    (define (helper term term-list)
      (if (= (order term) (length term-list))
          (cons (coeff term) term-list)
          (helper term (cons 0 term-list))))
    (if (=zero? (coeff term))
        term-list
        (helper term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list)
    (make-term
     (dec (length term-list))
     (car term-list)))
  (define (rest-terms term-list)
    ;; Remove leading zeros.
    (let ((rest (cdr term-list)))
      (if (empty-termlist? rest)
          (the-empty-termlist)
          (if (=zero? (car rest))
              (rest-terms rest)
              rest))))
  (define (empty-termlist? term-list)
    (null? term-list))
  (define (make-term order coeff)
    (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  ;; interface
  (define (tag x)
    (attach-tag 'dense x))
  (put 'adjoin-term 'dense
       (lambda (term term-list)
         (tag (adjoin-term term term-list))))
  (put 'the-empty-termlist '(dense)
       (lambda (t)
         (tag (the-empty-termlist))))
  (put 'first-term '(dense)
       first-term)
  (put 'rest-terms '(dense)
       (lambda (terms)
         (tag (rest-terms terms))))
  (put 'empty-termlist? '(dense)
       empty-termlist?)
  (put 'make 'dense
       (lambda (terms)
         (tag terms))))

(define (install-sparse-package)
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
  ;; interface
  (define (tag x)
    (attach-tag 'sparse x))
  (put 'adjoin-term 'sparse
       (lambda (term term-list)
         (tag (adjoin-term term term-list))))
  (put 'the-empty-termlist '(sparse)
       (lambda (t)
         (tag (the-empty-termlist))))
  (put 'first-term '(sparse)
       first-term)
  (put 'rest-terms '(sparse)
       (lambda (terms)
         (tag (rest-terms terms))))
  (put 'empty-termlist? '(sparse)
       empty-termlist?)
  (put 'make 'sparse
       (lambda (terms)
         (tag terms))))

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
  (define (make-term order coeff)
    (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (adjoin-term term term-list)
    ((get 'adjoin-term (type-tag term-list))
     term
     (contents term-list)))
  (define (empty-termlist? term-list)
    (apply-generic 'empty-termlist? term-list))
  (define (the-empty-termlist term-list)
    (apply-generic 'the-empty-termlist term-list))
  (define (first-term term-list)
    (apply-generic 'first-term term-list))
  (define (rest-terms term-list)
    (apply-generic 'rest-terms term-list))

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
        (let ((p1->string (symbol->string (variable p1)))
              (p2->string (symbol->string (variable p2))))
          (if (string<? p1->string p2->string)
              (add-poly p1 (make-poly (variable p1)
                                      (make-dense-term-list (list (tag p2)))))
              (add-poly p2 (make-poly (variable p2)
                                      (make-dense-term-list (list (tag p1)))))))))

  ;; =======
  ;; | MUL |
  ;; =======
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        L1
        (add-terms
         (mul-term-by-all-terms
          (first-term L1) L2)
         (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        L
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

  ;; =======
  ;; | DIV |
  ;; =======
  ;; Exercise 2.91
  ;; Division can be performed via long division.
  ;; That is, divide the highest-order term of the dividend by the highest-order
  ;; term of the divisor. The result is the first term of the quotient.
  ;; Next, multiply the result by the divisor, subtract that from the dividend,
  ;; and produce the rest of the answer by recursively dividing the difference
  ;; by the divisor. Stop when the order of the divisor exceeds the order
  ;; of the dividend and declare the dividend to be the remainder.
  ;; Also, if the dividend ever becomes zero, return zero as both quotient
  ;; and remainder.
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list L1 L1)
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist L1) L1)
              (let ((new-c (div (coeff t1)
                                (coeff t2)))
                    (new-o (- (order t1)
                              (order t2))))
                (let ((mul-result-divisor (mul-term-by-all-terms
                                           (make-term new-o new-c)
                                           L2)))
                  (let ((new-L1 (add-terms L1 (neg-terms mul-result-divisor))))
                    (let ((rest-of-result (div-terms new-L1 L2)))
                      (list (adjoin-term
                             (make-term new-o new-c)
                             (car rest-of-result))
                            (cadr rest-of-result))))))))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (div-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var:
              MUL-POLY"
               (list p1 p2))))

  ;; =======
  ;; | GCD |
  ;; =======
  (define (pseudoremainder-terms L1 L2)
    (let ((exponent (inc (- (order (first-term L1))
                            (order (first-term L2)))))
          (co (coeff (first-term L2))))
     (remainder-terms (mul-term-by-all-terms
                       (make-term 0 (expt co exponent)) L1)
                      L2)))
  (define (remainder-terms L1 L2)
    (cadr (div-terms L1 L2)))

  (define (gcd-coeffs terms)
    (if (empty-termlist? terms)
        0
        (gcd (coeff (first-term terms))
             (gcd-coeffs (rest-terms terms)))))
  (define (reduce-coeffs terms g)
    (if (empty-termlist? terms)
        terms
        (adjoin-term (make-term (order (first-term terms))
                                (/ (coeff (first-term terms)) g))
                     (reduce-coeffs (rest-terms terms) g))))
  (define (reduce-coeffs-single terms)
    (let ((g (gcd-coeffs terms)))
      (reduce-coeffs terms g)))
  (define (gcd-terms L1 L2)
    (define (helper L1 L2)
      (if (empty-termlist? L2)
          L1
          (helper L2 (pseudoremainder-terms L1 L2))))
    (reduce-coeffs-single (helper L1 L2)))
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (gcd-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var:
              GCD-POLY"
               (list p1 p2))))

  ;; ==========
  ;; | REDUCE |
  ;; =========
  (define (reduce-terms n d)
    (let ((g (gcd-terms n d))
          (exponent (inc (- (order (first-term n))
                            (order (first-term d)))))
          (co (coeff (first-term d))))
      (let ((factor (expt co exponent)))
        (let ((nn (car (div-terms
                        (mul-term-by-all-terms (make-term 0 factor) n)
                        g)))
              (dd (car (div-terms
                        (mul-term-by-all-terms (make-term 0 factor) d)
                        g))))
          (let ((g1 (gcd-coeffs nn))
                (g2 (gcd-coeffs dd)))
            (list (reduce-coeffs nn (gcd g1 g2))
                  (reduce-coeffs dd (gcd g1 g2))))))))

  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (let ((result
               (reduce-terms (term-list p1)
                             (term-list p2))))
          (list (make-poly (variable p1) (car result))
                (make-poly (variable p1) (cadr result))))
        (error "Polys not in same var:
              REDUCE-POLY"
               (list p1 p2))))

  ;; =======
  ;; | NEG |
  ;; =======
  ;; (Exercise 2.88)
  (define (neg-term term)
    (make-term (order term) (neg (coeff term))))
  (define (neg-terms terms)
    (if (empty-termlist? terms)
        terms
        (adjoin-term
         (neg-term (first-term terms))
         (neg-terms (rest-terms terms)))))
  (define (neg-polynomial p)
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
  (install-dense-package)
  (install-sparse-package)
  (put 'add '(polynomial real)
       (lambda (p r)
         (tag (add-poly p
                        (make-poly
                         (variable p)
                         (make-dense-term-list (list r)))))))
  (put 'add '(real polynomial)
       (lambda (r p)
         (tag (add-poly p
                        (make-poly
                         (variable p)
                         (make-dense-term-list (list r)))))))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (div-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms)
         (tag (make-poly var terms))))
  (put '=zero? '(polynomial)
       (lambda (p)
         (=zero-poly? p)))
  (put 'neg '(polynomial)
       (lambda (p)
         (tag (neg-polynomial p))))
  (put 'gcd '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (gcd-poly p1 p2))))
  (put 'reduce '(polynomial polynomial)
       (lambda (p1 p2)
         (let ((result (reduce-poly p1 p2)))
           (list (tag (car result))
                 (tag (cadr result))))))
  'done)

(install-polynomial-package)
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
(define (make-dense-term-list terms)
  ((get 'make 'dense) terms))
(define (make-sparse-term-list terms)
  ((get 'make 'sparse) terms))

;; Test
(define p1 (make-polynomial
            'x
            (make-sparse-term-list
             (list (list 1 2) (list 0 3)))))
(define p2 (make-polynomial
            'x
            (make-dense-term-list '(4 0 5))))
(define p3 (make-polynomial
            'y
            (make-sparse-term-list
             (list (list 2 p1) (list 0 p2)))))

(if (equal? (add p1 p1)
            (make-polynomial
            'x
            (make-sparse-term-list
             (list (list 1 4) (list 0 6)))))
    #t
    (error "(add p1 p1)"))

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
(=zero? (make-polynomial
         'x
         (make-dense-term-list '(0 0)))) ;; #t
(=zero? (make-polynomial
         'x
         (make-sparse-term-list
          (list '(1 2) '(0 3))))) ;; #f
(=zero? (make-polynomial
         'x
         (make-sparse-term-list
          (list '(1 0) '(0 0))))) ;; #t

;; Exercise 2.88
(if (equal? (neg p1)
            (make-polynomial
             'x
             (make-sparse-term-list
              (list (list 1 -2) (list 0 -3)))))
    #t
    (error "(neg p1)"))

(if (equal? (neg p2)
            (make-polynomial
             'x
             (make-dense-term-list '(-4 0 -5))))
    #t
    (error "(neg p2)"))

(define (sub-poly p1 p2)
  (add p1 (neg p2)))

(sub-poly p1 p2)
(sub-poly p2 p1)

;; Exercise 2.89
;; Implemented above.

;; Exercise 2.90
;; Implemented above.

;; Exercise 2.91
;; Implemented above.
(define dividend (make-polynomial
                  'x
                  (make-sparse-term-list (list '(5 1) '(0 -1)))))
(define divisor (make-polynomial
                 'x
                 (make-dense-term-list '(1 0 -1))))
(div dividend divisor)

;; Exercise 2.92
;; Only implementing this for add.
(define px (make-polynomial
            'x
            (make-sparse-term-list
             (list (list 1 2) (list 0 3)))))
(define py (make-polynomial
            'y
            (make-sparse-term-list
             (list (list 1 2) (list 0 3)))))
(add px py)
;; (polynomial x dense 2 (polynomial y dense 2 6))
(add py (add px (add px py)))
;; (polynomial x dense 4 (polynomial y sparse (1 4) (0 12)))


;; Exercise 2.93
;; Use Rational instead of Rational to avoid conflicts with Rational package.
(define (install-rational-package)
  ;; internal procedures
  (define (numer r) (car r))
  (define (denom r) (cdr r))
  (define (add-rational a b)
    (make-rat (add (mul (numer a) (denom b))
                   (mul (numer b) (denom a)))
              (mul (denom a) (denom b))))
  (define (rational->real r)
    (make-real (/ (numer r) (denom r))))
  (define (make-rat n d)
    (let ((result (reduce n d)))
      (cons (car result) (cadr result))))
  (define (equ? r1 r2)
    (= (* (numer r1) (denom r2))
       (* (denom r1) (numer r2))))
  ;; interface
  (define (tag x)
    (attach-tag 'Rational x))
  (put 'add '(Rational Rational)
       (lambda (a b)
         (tag (add-rational a b))))
  (put 'make-rat 'Rational
       (lambda (a b)
         (tag (make-rat a b))))
  (put 'equ? '(Rational Rational)
       equ?)
  (put '=zero? '(Rational)
       (lambda (x) (= (numer x) 0)))
  'done)
(install-rational-package)
(define (make-rational a b)
  ((get 'make-rat 'Rational) a b))

(define p1 (make-polynomial 'x (make-sparse-term-list '((1 2) (0 1)))))
(define p2 (make-polynomial 'x (make-sparse-term-list '((1 3) (0 1)))))
(define rf (make-rational p2 p1))
(add rf rf)
;; (Rational (polynomial x sparse (2 12) (1 10) (0 2))
;;            polynomial x sparse (2 4) (1 4) (0 1))

;; Exercise 2.94
(define p1
  (make-polynomial
   'x (make-sparse-term-list '((4 1) (3 -1) (2 -2) (1 2)))))

(define p2
  (make-polynomial
   'x (make-sparse-term-list '((3 1) (1 -1)))))

(gcd p1 p2) ;; (polynomial x sparse (2 -1) (1 1))

;; Exercise 2.95
(define p1
  (make-polynomial
   'x (make-dense-term-list '(1 -2 1))))
(define p2
  (make-polynomial
   'x (make-dense-term-list '(11 0 7))))
(define p3
  (make-polynomial
   'x (make-dense-term-list '(13 5))))

(define q1 (mul p1 p2))
(define q2 (mul p1 p3))
(gcd q1 q2) ;;  (polynomial x dense 1458/169 -2916/169 1458/169)
;; Different from p1 but since the degree is the same, both are equally
;; valid gcds. Why does this happen? Looks like it gets multipled by the
;; gcd of p2 and p3.
(gcd p2 p3) ;; (polynomial x dense 1458/169)

;; Exercise 2.96a
(gcd q1 q2) ;; (polynomial x dense 1458 -2916 1458)

;; Exercise 2.96b
(gcd q1 q2) ;;  (polynomial x dense 1 -2 1)

;; Exercise 2.97
(define p1
  (make-polynomial 'x (make-sparse-term-list '((1 1) (0 1)))))
(define p2
  (make-polynomial 'x (make-sparse-term-list '((3 1) (0 -1)))))
(define p3
  (make-polynomial 'x (make-sparse-term-list '((1 1)))))
(define p4
  (make-polynomial 'x (make-sparse-term-list '((2 1) (0 -1)))))
(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))

(add rf1 rf2)
;; (Rational (polynomial x sparse (3 -1) (2 -2) (1 -3) (0 -1))
;;            polynomial x sparse (4 -1) (3 -1) (1 1) (0 1))
