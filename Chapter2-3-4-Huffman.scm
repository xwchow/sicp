#!/usr/bin/guile
!#

(add-to-load-path ".")
(use-modules (util))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

;; Exercise 2.67
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch 
                (car bits) 
                current-branch)))
          (if (leaf? next-branch)
              (cons 
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) 
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: 
               CHOOSE-BRANCH" bit))))

(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree) ;; (A D A B B C A)

;; Exercise 2.68
(define (encode-symbol sym tree)
  (if (memq sym (symbols tree))
      (if (leaf? tree)
          '()
          (let ((left (left-branch tree))
                (right (right-branch tree)))
            (if (memq sym (symbols left))
                (cons 0 (encode-symbol sym left))
                (cons 1 (encode-symbol sym right)))))
      (error "Symbol not in tree:" sym)))

(define (encode message tree)
  (if (null? message)
      '()
      (append 
       (encode-symbol (car message) 
                      tree)
       (encode (cdr message) tree))))

(equal? (encode '(A D A B B C A) sample-tree) sample-message)

;; Exercise 2.69
(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))

(define (successive-merge nodes)
  (if (single? nodes)
      (car nodes)
      (let ((x (car nodes))
            (y (cadr nodes)))
        (let ((xy (make-code-tree x y)))
          (successive-merge (adjoin-set xy (cddr nodes)))))))

(define huffman-tree (generate-huffman-tree
                      (list '(A 8) '(B 3) '(C 1) '(D 1) 
                            '(E 1) '(F 1) '(G 1) '(H 1))))

(define message '(B A C A D A E A F A B B A A A G A H))
(equal? message (decode (encode message huffman-tree) huffman-tree)) ;;  #t
(length (encode '(B A C A D A E A F A B B A A A G A H) huffman-tree)) ;;  42

;; Exercise 2.70
(define rock-huffman-tree (generate-huffman-tree
                           (list '(A 2) '(NA 16)
                                 '(BOOM 1) '(SHA  3)
                                 '(GET  2) '(YIP  9)
                                 '(JOB  2) '(WAH  1))))

(define rock-message '(GET A JOB
                           SHA NA NA NA NA NA NA NA NA

                           GET A JOB
                           SHA NA NA NA NA NA NA NA NA

                           WAH YIP YIP YIP YIP 
                           YIP YIP YIP YIP YIP
                           SHA BOOM))

(length (encode rock-message rock-huffman-tree)) ;;  84
(* 3 (length rock-message)) ;; 108
;; 84 versus 108 for fixed-length encoding

;; Exercise 2.71
;;
;;        {ABCDE} 31
;;        /       \
;;      /          \
;;    A 16         {BCDE} 15
;;                /       \
;;              /          \
;;            B 8       {CDE} 7
;;                       /    \
;;                     /       \
;;                   C 4       {DE} 3
;;                             /   \
;;                           /      \
;;                         D 2      E 1
;;
;; In general, for 1 bit for the most frequent and (n-1) bits for the least

;; Exercise 2.72
;; Worst case is O(n^2) due to O(n) levels and O(n) searching.

