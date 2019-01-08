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
  (define (dispatch entered-password m)
    (cond ((not (eq? entered-password password)) (lambda (x) "Incorrect password"))
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
    (define (dispatch entered-password m)
      (if (not (eq? entered-password password))
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
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral pred x1 x2 y1 y2 trials)
  (define (exp)
    (pred (random-in-range x1 (* 1. x2)) (random-in-range y1 (* 1. y2))))
  (let ((area (* (- x2 x1) (- y2 y1)))
        (pct (monte-carlo trials exp)))
    (* pct area)))

(define (estimate-pi trials)
  ;; Use area of circle = PI*R^2
  (let ((radius 3) (xc 5) (yc 7))
    (let ((pred (lambda (x y)
                  (<= (+ (square (- x xc))
                         (square (- y yc)))
                      (square radius)))))
      (let ((area-of-circle (estimate-integral pred 2 8 4 10 trials)))
        (/ area-of-circle (square radius))))))

(* 1. (estimate-pi 100000)) ;; 3.14532

;; Exercise 3.6
(define rand
  (let ((a 339639331) (b 26936383) (m 1154749949))
   (define (rand-update x)
     (remainder (+ b (* a x)) m))
   (let ((x (random-in-range 0 m)))
     (define generate
       (lambda () (set! x (rand-update x)) x))
     (define reset
       (lambda (reset-value)
         (set! x reset-value)
         x))
     (define (dispatch msg)
       (cond ((eq? msg 'generate)
              (generate))
             ((eq? msg 'reset)
              reset)
             (else (error "Unsupported operation:" msg))))
     dispatch)))

(rand 'generate)
((rand 'reset) 1)

;; Exercise 3.7
(define (make-account balance password)
  (let ((passwords (list password)))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance
                   (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (verify entered-password)
      (auth entered-password))
    (define (add-password new-password)
      (set! passwords (cons new-password passwords)))
    (define (auth p)
      (member p passwords))
    (define (dispatch entered-password m)
      (cond ((not (auth entered-password)) (lambda (x) "Incorrect password"))
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'verify) verify)
            ((eq? m 'add-password) add-password)
            (else (error "Unknown request:
                 MAKE-ACCOUNT" m))))
    dispatch))

(define (make-joint account password joint-password)
  (if (account 'verify password)
      (begin ((account password 'add-password) joint-password)
             account)
      (error "Wrong password:" password)))
(define peter-acc (make-account 100 'open-sesame))
(define paul-acc
  (make-joint peter-acc
              'open-sesame
              'rosebud))

((paul-acc 'rosebud 'deposit) 100)
((peter-acc 'open-sesame 'withdraw) 200)

;; Exercise 3.8
(define f
  ;; Returns a function that remembers the previous value.
  (let ((prev 0))
    (lambda (x)
      (let ((tmp prev))
        (set! prev x)
        tmp))))

(+ (f 0) (f 1))

;; Exercise 3.9
;; Recursive:
;; Global env has factorial binded
;; E1: n = 6 - finds factorial in global
;; E2: n = 5
;; ...

;; Iterative:
;; Global env has factorial and fact-iter binded
;; E1: n = 6 - finds fact-iter in global
;; E2: product = 1, counter = 1, max-count = 6
;; E2: product = 1, counter = 2, max-count = 6

;; Exercise 3.10
;;
;;            ----------------------
;; global env | make-withdraw      |
;;            | W1                 |
;;            ----------------------
;;
;;    -----------------------
;; E1 | initial_amount: 100 |
;;    ----------------------
;;
;;    -----------------------
;; E2 | balance: 100        |
;;    ----------------------
;;
;; W1 -> parameter: amount
;;       body: (if (>= balance amount)
;;                 (begin (set! balance
;;                              (- balance amount))
;;                        balance)
;;                 "Insufficient funds")
;;
;; (W1 50) decrements balance in E2 by 50

;; Exercise 3.11
;; Done but not here.

;; Exercise 3.12
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
(cdr x) ;; => (b)

(define w (append! x y))
(cdr x) ;; => (b c d)

;; Exercise 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))
;; (last-pair z) infinite loops

;; Exercise 3.14
;; mystery reverses a list!
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))
;; w: '(a b c d)
;; v: '(a)
