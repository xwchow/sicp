#!/usr/bin/guile
!#

(add-to-load-path ".")
(use-modules (util))

;; Hierarchical Data and the Closure Property
;; Exercise 2.17
(define (my-last-pair list)
  (if (null? (cdr list))
      list
      (my-last-pair (cdr list))))
(my-last-pair (list 23 72 149 34))

;; Exercise 2.18
(define (my-reverse list1)
  (if (null? list1)
      list1
      (append (my-reverse (cdr list1))
              (list (car list1)))))
(my-reverse (list 1 4 9 16 25))

;; Exercise 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (cc amount coin-values)
  (define (no-more? list1) (null? list1))
  (define (except-first-denomination list1) (cdr list1))
  (define (first-denomination list1) (car list1))
  (cond ((= amount 0) 
         1)
        ((or (< amount 0) 
             (no-more? coin-values)) 
         0)
        (else
         (+ (cc 
             amount
             (except-first-denomination 
              coin-values))
            (cc 
             (- amount
                (first-denomination 
                 coin-values))
             coin-values)))))
(cc 100 us-coins)

;; Exercise 2.20
(define (same-parity x . y)
  (define (helper a b) ;; returns (b) if a == b (mod 2)
    (if (eq? (even? a) (even? b))
        (list b)
        (list)))
  (define (same-parity-list list1)
    (if (null? list1)
        list1
        (append (helper x (car list1))
                (same-parity-list (cdr list1)))))
  (append (list x) (same-parity-list y)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
