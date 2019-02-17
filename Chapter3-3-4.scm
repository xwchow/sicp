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
          (else (let ((c-out (make-wire)))
                  (full-adder (car A) (car B) c-in (car S) c-out)
                  (helper (cdr A) (cdr B) c-out (cdr S))))))
  (helper A B (make-wire) S))

;; So, the way this works as I understand it
;; Gates: perform a logical operation and returns a procedure encapsulating
;;        that operation.
;; Wires: Stateful. Boolean state and list of procedures to run. In order
;;        to model delay, we push procedures onto the agenda.
;; Agenda: Global queue of actions.
(define (after-delay delay action)
  (add-to-agenda!
   (+ delay (current-time the-agenda))
   action
   the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item
             (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action!
   wire
   (lambda ()
     (newline)
     (display name)
     (display " ")
     (display (current-time the-agenda))
     (display "  New-value = ")
     (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

;; Exercise 3.31
;; Values of output will not be initialised.
;; Will remain 0 since the default of (make-wire) is 0.

;; Agenda code
(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda)
  (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time
           (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue!
         (segment-queue (car segments))
         action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment
                      time
                      action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment
                time
                action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue
            (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments!
         agenda
         (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty:
              FIRST-AGENDA-ITEM")
      (let ((first-seg
             (first-segment agenda)))
        (set-current-time!
         agenda
         (segment-time first-seg))
        (front-queue
         (segment-queue first-seg)))))

;; Exercise 3.32
;; Let the wire names be [a & b = c] with initial values being [0, 1, 0]
;; In a FIFO queue:
;; queue = [b->0 a->1]    state = [0, 1, 0]
;; queue = [a->1 | c->0]  state = [0, 0, 0]
;; queue = [c->0 c->0]    state = [1, 0, 0]
;; queue = [c->0]         state = [1, 0, 0]
;; queue = []             state = [1, 0, 0]
;;
;; In a LIFO list:
;; list = [b->0 a->1]    state = [0, 1, 0]
;; list = [b->0 | c->1]  state = [1, 1, 0]
;; list = [c->1 c->0]    state = [1, 0, 0]
;; list = [c->1]         state = [1, 0, 0]
;; list = []             state = [1, 0, 1] <= c is WRONG
