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

;; Exercise 3.15
;; Done. Not here.

;; Exercise 3.16
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; returns 3
(define c (cons 'c 'd))
(define b (cons 'b c))
(define a (cons 'a b))
(count-pairs a)

;; returns 4
(define c (cons 'c 'd))
(define b (cons c c))
(define a (cons 'a b))
(count-pairs a)

;; returns 7
(define c (cons 'c 'd))
(define b (cons c c))
(define a (cons b b))
(count-pairs a)

;; never return
(define c (cons 'c 'd))
(define b (cons 'b c))
(define a (cons 'a b))
(set-cdr! c a)

;; Exercise 3.17
(define (count-pairs x)
  (let ((seen-pairs '()))
    (define (seen? x)
      (member x seen-pairs))
    (define (helper x)
      (cond ((seen? x) 0)
            ((not (pair? x)) 0)
            (else
             (set! seen-pairs (cons x seen-pairs))
             (+ (helper (car x))
                (helper (cdr x))
                1))))
    (helper x)))

;; used to return 3
(define c (cons 'c 'd))
(define b (cons 'b c))
(define a (cons 'a b))
(count-pairs a)

;; used to return 4
(define c (cons 'c 'd))
(define b (cons c c))
(define a (cons 'a b))
(count-pairs a)

;; returns 7
(define c (cons 'c 'd))
(define b (cons c c))
(define a (cons b b))
(count-pairs a)

;; never return
(define c (cons 'c 'd))
(define b (cons 'b c))
(define a (cons 'a b))
(set-cdr! c a)
(count-pairs a)

;; Exercise 3.18
(define (has-cycle? x)
  (let ((seen-elems '()))
    (define (seen? x)
      (member x seen-elems))
    (define (helper x)
      (cond ((not (pair? x)) #f)
            ((seen? (car x)) #t)
            (else
             (set! seen-elems (cons (car x) seen-elems))
             (helper (cdr x)))))
    (helper x)))

(define a (list 1 2 3 4))
(has-cycle? a)

(define z (make-cycle (list 'a 'b 'c)))
(has-cycle? z)

;; Exercise 3.19
;; Floyd's Cycle Finding algorithm
;; Create two pointers:
;; The first pointer advances two steps each iteration
;; The second pointer advances one step each iteration
;; If the first pointer ever catches the second pointer, we have found a cycle.
(define (has-cycle? x)
  (define (helper x y)
    (cond ((not (pair? x)) #f)
          ((not (pair? (cdr x))) #f)
          ((eq? (cdr x) y) #t)
          ((eq? (cdr (cdr x)) y) #t)
          (else 
           (helper (cdr (cdr x)) (cdr y)))))
  (helper x x))

