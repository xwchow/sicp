(define (inverter input output)
  (define (invert-input)
    (let ((new-value
           (logical-not (get-signal input))))
      (after-delay
       inverter-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1)
                        (get-signal a2))))
      (after-delay
       and-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (cond (((and (= s1 0) (= s2 0)) 0)
         ((and (= s1 0) (= s2 1)) 0)
         ((and (= s1 1) (= s2 0)) 0)
         ((and (= s1 1) (= s2 1)) 1)
         (else (error "Invalid signal" s1 s2)))))

;; Exercise 3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1)
                       (get-signal a2))))
      (after-delay
       or-gate-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond (((and (= s1 0) (= s2 0)) 0)
         ((and (= s1 0) (= s2 1)) 1)
         ((and (= s1 1) (= s2 0)) 1)
         ((and (= s1 1) (= s2 1)) 1)
         (else (error "Invalid signal" s1 s2)))))

;; Exercise 3.29
;; De Morgan's law
;; 2 inverter-delay + 1 and-gate-delay
(define (or-gate a1 a2 output)
  (let ((not-a1 (make-wire))
        (not-a2 (make-wire))
        (not-output (make-wire)))
    (inverter a1 not-a1)
    (inverter a2 not-a2)
    (and-gate a1 a2 not-output)
    (inverter not-output output)))

;; Exercise 3.30
;; The delay is:
;;   n * (full-adder-delay)
;; = n * (half-adder-delay + or-gate-delay)
;; = n * (max(and + inverter, or) + and + or)
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((c1 (make-wire))
        (c2 (make-wire))
        (s  (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder A B C S)
  (define (helper A B c-in S)
    (cond ((null? (cdr A))
           (full-adder (car A) (car B) c-in (car S) C))
          (else (define c-out (make-wire))
                (full-adder (car A) (car B) c-in (car S) c-out)
                (helper (cdr A) (cdr B) c-out (cdr S)))))
  (helper A B (make-wire) S))

