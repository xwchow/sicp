#!/usr/bin/guile
!#

(add-to-load-path ".")
(use-modules (util))

;; Exercise 3.1
(define (make-accumulator total)
  (define (accum arg)
    (set! total (+ total arg))
    total)
  accum)
(define A (make-accumulator 5))
(A 10)
(A 10)

;; Exercise 3.2
(define (make-monitored f)
  ;; f is a single argument function.
  (let ((num-calls 0))
    (define (how-many-calls?)
      num-calls)
    (define (reset-count)
      (set! num-calls 0)
      num-calls)
    (define (apply-f arg)
      (set! num-calls (inc num-calls))
      (f arg))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) (how-many-calls?))
            ((eq? m 'reset-count) (reset-count))
            (else (apply-f m))))
    dispatch))

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)

;; Exercise 3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch auth m)
    (cond ((not (eq? auth password)) (lambda (x) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request:
                 MAKE-ACCOUNT" m))))
  dispatch)

(define acc
  (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)

;; Exercise 3.4
(define (make-account balance password)
  (let ((incorrect-attempts 0))
    (define (call-the-cops)
      (print "WOO WOO CALLING THE COPS")
      (lambda (x) "THEY ARE COMING FOR YOU"))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance
                   (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch auth m)
      (if (not (eq? auth password))
          (begin
            (set! incorrect-attempts (inc incorrect-attempts))
            (if (> incorrect-attempts 7)
                (call-the-cops)
                (lambda (x) "Incorrect password")))
          (begin
            (set! incorrect-attempts 0)
            (cond
             ((eq? m 'withdraw) withdraw)
             ((eq? m 'deposit) deposit)
             (else (error "Unknown request:
                 MAKE-ACCOUNT" m))))))
    dispatch))

(define acc
  (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)

;; Exercise 3.5
