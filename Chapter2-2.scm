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
  (cons x (same-parity-list y)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

;; Exercise 2.21
(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
            (cdr items))))

(define (square-list items)
  (map square items))

(square-list '(1 2 3 4))

;; Exercise 2.22
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things)) ;; appends to front of list
                    answer))))
  (iter items #nil))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square
                     (car things)))))) ;; this forms a pair (list, element) instaed of a new list
  (iter items #nil))

;; Exercise 2.23
(define (my-for-each proc items)
  (cond ((null? items)
         #nil)
        (else
         (proc (car items))
         (my-for-each proc (cdr items)))))

(my-for-each
 (lambda (x) (newline) (display x))
 (list 57 321 88))

;; Exercise 2.24
(list 1 (list 2 (list 3 4)))
; => (1 (2 (3 4)))

;; Exercise 2.25
(define a (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr a)))))

(define a (list (list 7)))
(car (car a))

(define a (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(cadr (cadr (cadr (cadr (cadr (cadr a))))))

;; Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(cons x y)
(list x y)

;; Exercise 2.27
(define x (list (list 1 2) (list 3 4)))
(my-reverse x)
(define (deep-reverse x)
  (cond ((null? x) x)
        ((not (pair? x)) x)
        (else (append (deep-reverse (cdr x))
                      (list (deep-reverse (car x)))))))
(print (deep-reverse x))

;; Exercise 2.28
(define x (list (list 1 2) (list 3 4)))
(define (fringe x)
  (cond ((null? x) x)
        ((not (pair? x)) (list x))
        (else (append (fringe (car x))
                      (fringe (cdr x))))))

(fringe x)
(fringe (list x x))

;; Exercise 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

;; a)
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

;; b)
(define (branch-weight branch)
  (total-weight (branch-structure branch)))
(define (total-weight mobile)
  (if (atom mobile)
      mobile
      (+ (branch-weight (left-branch mobile))
         (branch-weight (right-branch mobile)))))

(define mob1 (make-mobile
              (make-branch 1 9)
              (make-branch 3 3)))
(define mob2 (make-mobile
              (make-branch 5 mob1)
              (make-branch 6 7)))
(total-weight mob2)

;; c)
(define (torque branch)
  (* (branch-length branch)
     (total-weight (branch-structure branch))))

(define (is-balanced-branch branch)
  (is-balanced (branch-structure branch)))

(define (is-balanced mobile)
  (if (atom mobile)
      #t
      (let ((left (left-branch mobile))
            (right (right-branch mobile)))
        (and (is-balanced-branch left)
             (is-balanced-branch right)
             (= (torque left)
                (torque right))))))

(is-balanced mob1)
(is-balanced mob2)

;; d)
;; Change cadr to cdr in accessors

;; Exercise 2.30
(define (square-tree tree)
  (cond ((null? tree) #nil)
        ((atom tree) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-2 tree)
  (map (lambda (sub-tree)
         (if (atom sub-tree)
             (square sub-tree)
             (square-tree-2 sub-tree)))
       tree))

(square-tree-2
 (list 1
       (list 2 (list 3 4) 5 )
       (list 6 7)))

;; Exercise 2.31
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (atom sub-tree)
             (f sub-tree)
             (tree-map f sub-tree)))
       tree))

(define (square-tree-3 tree) (tree-map square tree))

(square-tree-3
 (list 1
       (list 2 (list 3 4) 5 )
       (list 6 7)))

;; Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list #nil)
      (let ((fst (car s))
            (rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons fst x))
                          rest)))))

(subsets (list 1))
(subsets (list 1 2 3))

;; Exercise 2.33
(define (map-2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              #nil
              sequence))

(map-2 square (list 1 2 3 4))

(define (append-2 seq1 seq2)
  (accumulate cons
              seq2
              seq1))

(append-2 (list 2 4 6) (list 3 5 7))

(define (length-2 sequence)
  (accumulate (lambda (x y) (1+ y))
              0
              sequence))

(length-2 (list 1 2 4 5))

;; Exercise 2.34
(define (horner-eval x coeff-sequence)
  (accumulate (lambda (a b)
                (+ a (* b x)))
              0
              coeff-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;; Exercise 2.35
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x)
                     (if (atom x)
                         1
                         (count-leaves x)))
                   t)))

(count-leaves (list 1 2 (list (list 3 4) 5 (list 6 7 8))))

;; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      #nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 (list (list 2 3 4) (list 5 4 3)))

;; Exercise 2.37
(define M (list '(1 2 3 4) '(4 5 6 6) '(6 7 8 9)))
(define (dot-product v w)
  (accumulate +
              0
              (map * v w)))
(define (matrix-*-vector m v)
   (map (lambda(w)
          (dot-product w v)) m))
(define (transpose m)
  (accumulate-n cons (list) m))
(transpose M)
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
   (map (lambda(w)
          (matrix-*-vector cols w)) m)))
(matrix-*-matrix M (transpose M))

;; Exercise 2.38
(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3)) ;; 3/2
(fold-left / 1 (list 1 2 3)) ;; 1/6
(fold-right list #nil (list 1 2 3)) ;; (1 (2 (3 #nil)))
(fold-left list #nil (list 1 2 3)) ;; (((#nil 1) 2) 3)
;; op should be associative

;; Exercise 2.39
(define (reverse sequence)
  (fold-right (lambda(x y)
                (append y (list x))) '() sequence))
(define (reverse sequence)
  (fold-left (lambda(x y)
               (cons y x)) '() sequence))

(reverse '(1 2 3))
(reverse '(4 5 6))

;; Nested mappings
(define (flatmap proc seq)
  (accumulate append #nil (map proc seq)))
(define (gen-pairs n)
  (flatmap (lambda (i)
           (map (lambda (j)
                  (list i j))
                (enumerate-interval 1 (- i 1))))
         (enumerate-interval 1 n)))
(define (prime-sum? pair)
  (and (prime? (+ (car pair) (cadr pair)))
       (prime? (car pair))
       (prime? (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair) (cadr pair))))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (gen-pairs n))))
(prime-sum-pairs 1000)

(define (permutations S)
  (if (null? S)
      (list #nil)
      (flatmap (lambda (x)
                 (map (lambda (perm)
                            (cons x perm))
                          (permutations (delete x S))))
               S)))

(permutations '(1 2 3))
(permutations '(1 2 3 4))

;; Exercise 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
           (map (lambda (j)
                  (list i j))
                (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))
(prime-sum-pairs 100)

;; Exercise 2.41
(define (ordered-doubles n)
    (flatmap (lambda (i)
           (map (lambda (j)
                  (list i j))
                (enumerate-interval 1 n)))
             (enumerate-interval 1 n)))
(define (ordered-triples n)
  (flatmap (lambda (j)
             (map (lambda (i)
                    (cons i j))
                  (enumerate-interval 1 n)))
           (ordered-doubles n)))

(define (sum-triples n s)
  (let ((triples (ordered-triples n)))
    (filter (lambda (t)
              (= s (accumulate + 0 t))) triples)))
(sum-triples 3 7)

;; Exercie 2.42
(define (queens board-size)
  ;; The board structure is represented as a list of ints representing
  ;; the row position and indexed by column position
  (define empty-board (list))
  (define (adjoin-position row col positions)
    (cons row positions))
  (define (unique? positions)
    (let ((x (car positions)))
      ;; Would be nicer if we had an all method.
      (= 0 (accumulate + 0 (map (lambda (y)
                                  (if (= x y) 1 0))
                                (cdr positions))))))
  (define (safe? col positions)
    ;; The board is safe if there are no queens in the same row
    ;; and no queens in the same diagonal (check this by subtracting/adding
    ;; their individual column index)
    (and (unique? positions)
         (unique? (map - positions (reverse (enumerate-interval 1 col))))
         (unique? (map - positions (enumerate-interval 1 col)))))
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (dec k))))))
  (queen-cols board-size))

(length (queens 8))

;; Exercise 2.43
;; queen-cols is called N times for a board-size of N
;; O(N!) time

;; Exercise 2.44
(define (up-split painter)
  (below (beside painter painter)
         painter))

;; Exercise 2.45
(define (split f1 f2 painter)
  (f1 (f2 painter painter)
      painter))

;; Exercise 2.46
(define (make-vect x y)
  (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cadr v))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect v s)
  (make-vect (* (xcor-vect v) s)
             (* (ycor-vect v) s)))


;; Exercise 2.47a
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

;; Exercise 2.47b
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cddr frame))

;; Exercise 2.48
(define (make-segment v1 v2)
  (cons v1 v2))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

;; Exercise 2.49
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) 
         (start-segment segment))
        ((frame-coord-map frame) 
         (end-segment segment))))
     segment-list)))

;; The painter that draws the outline of the designated frame.
(define outline-painter (segments->painter
                         ((make-segment (make-vect 0 0)
                                        (make-vect 0 1))
                          (make-segment (make-vect 0 0)
                                        (make-vect 1 0))
                          (make-segment (make-vect 0 1)
                                        (make-vect 1 1))
                          (make-segment (make-vect 1 0)
                                        (make-vect 1 1)))))

;; The painter that draws an “X” by connecting opposite corners of the frame.
(define X-painter (segments->painter
                         ((make-segment (make-vect 0 0)
                                        (make-vect 1 1))
                          (make-segment (make-vect 0 1)
                                        (make-vect 1 0)))))

;; The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
(define diamond-painter (segments->painter
                         ((make-segment (make-vect 0.5 0)
                                        (make-vect 1 0.5))
                          (make-segment (make-vect 1 0.5)
                                        (make-vect 0.5 1))
                          (make-segment (make-vect 0.5 1)
                                        (make-vect 0 0.5))
                          (make-segment (make-vect 0 0.5)
                                        (make-vect 0.5 0)))))

;; TODO: The wave painter.

;; Transform painter
(define (transform-painter 
         painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                  (sub-vect (m corner1) 
                            new-origin)
                  (sub-vect (m corner2)
                            new-origin)))))))

;; Exercise 2.50
(define (flip-horiz painter)
  (transform-painter
   painter
   (make-vect 1 0)
   (make-vect 0 0)
   (make-vect 0 1)))

(define (rotate180 painter)
  (transform-painter
   painter
   (make-vect 1 1)
   (make-vect 0 1)
   (make-vect 1 0)))

(define (rotate270 painter)
  (transform-painter
   painter
   (make-vect 1 0)
   (make-vect 1 1)
   (make-vect 0 0)))

;; Exercise 2.51a
(define (below painter1 painter2)
  (let ((paint-top (transform-painter
                    painter2
                    (make-vect 0 0.5)
                    (make-vect 1 0.5)
                    (make-vect 0 1)))
        (paint-bottom (transform-painter
                    painter1
                    (make-vect 0 0)
                    (make-vect 1 0)
                    (make-vect 0 0.5)))))
  (lambda (frame)
    (paint-top frame)
    (paint-bottom frame)))

;; Exercise 2.51b
(define (below painter1 painter2)
  (rotate270 (beside (rotate90 painter1)
                     (rotate90 painter2))))

;; TODO: Exercise 2.52
