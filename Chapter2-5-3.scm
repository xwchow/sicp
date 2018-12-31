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
        (error "Polys not in same var:
              ADD-POLY"
               (list p1 p2))))

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
    (print (list "div-terms" L1 L2))
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
                (print (list "new-co" new-c new-o))
                (let ((mul-result-divisor (mul-term-by-all-terms
                                           (make-term new-o new-c)
                                           L2)))
                  (print (list "mul-result-divisor" mul-result-divisor))
                  (let ((new-L1 (add-terms L1 (neg-terms mul-result-divisor))))
                    (print (list "new-L1" new-L1))
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


