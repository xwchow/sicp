(define-module (util)
  #:export (square
            cube
            print
            divides?
            smallest-divisor
            prime?
            average
            atom))

(define (square x) (* x x))
(define (cube x) (* x x x))
(define (print x)
  (display x)
  (newline))

(define (divides? a b)
  (= (remainder b a) 0))
(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))
(define (prime? n)
  (if (< n 2)
      #f
      (= n (smallest-divisor n))))

(define (average x y)
  (/ (+ x y) 2))

(define (atom x) (not (pair? x)))
