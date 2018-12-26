#!/usr/bin/guile
!#

(add-to-load-path ".")
(use-modules (util))

(define (memq item x)
  (cond ((null? x) #false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple '(pear banana prune))
(memq 'apple '(x (apple sauce) y apple pear))

;; Exercise 2.53
(list 'a 'b 'c) ;; (a b c)
(list (list 'george)) ;; ((george))
(cdr '((x1 x2) (y1 y2))) ;;  ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ;;  (y1 y2)
(pair? (car '(a short list))) ;;  #f
(memq 'red '((red shoes) (blue socks))) ;;  #f
(memq 'red '(red shoes blue socks)) ;;  (red shoes blue socks)

;; Exercise 2.54
(define (my-equal? a b)
  (cond ((and (nil? a) (nil? b)) #t)
        ((nil? a) #f)
        ((nil? b) #f)
        (else (and (eq? (car a) (car b))
                   (my-equal? (cdr a) (cdr b))))))

(my-equal? '(this is a list)
           '(this is a list))

(equal? '(this is a list)
        '(this (is a) list))

;; Exercise 2.55
(car ''abracadabra)
;; Equivalent to:
(car (quote (quote abracadabra)))

;; Differentiation program
;; (variable? e)          Is e a variable?
;; (same-variable? v1 v2) Are v1 and v2 the same variable?
;; (sum? e)               Is e a sum?
;; (addend e)             Addend of the sum e.
;; (augend e)             Augend of the sum e.
;; (make-sum a1 a2)       Construct the sum of a1 and a2.
;; (product? e)           Is e a product?
;; (multiplier e)         Multiplier of the product e.
;; (multiplicand e)       Multiplicand of the product e.
;; (make-product m1 m2)   Construct the product of m1 and m2.
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

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        (else (error "unknown expression
                      type: DERIV" exp))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((=number? m1 0) 0)
        ((=number? m2 0) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

;; Exercise 2.56
;; Implement exponentiation rule. Build in the rules that:
;; anything raised to the power 0 is 1
;; anything raised to the power 1 is the thing itself
(define (make-exponentiation b exp)
  (cond ((= exp 0) 1)
        ((= exp 1) b)
        (else (list '** b exp))))
(define (exponentiation? e)
  (and (pair? e) (eq? '** (car e))))
(define (base e)
  (cadr e))
(define (exponent e)
  (caddr e))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp) (dec (exponent exp))))
          (deriv (base exp) var)))
        (else (error "unknown expression
                      type: DERIV" exp))))

(deriv '(* 3 (** x 3)) 'x)
(deriv '(* 3 (** x 1)) 'x)
(deriv '(* 3 (** x 0)) 'x)

;; Exercise 2.57
;; make-product just works
(define (multiplicand e)
  (let ((rest (cddr e)))
    (if (single? rest)
        (car rest)
        (cons '* rest))))
(define (augend e)
  (let ((rest (cddr e)))
    (if (single? rest)
        (car rest)
        (cons '+ rest))))

(deriv '(* 3 x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(* x y (+ x 3)) 'x)

;; Exercise 2.58a
(define (product? e)
  (and (pair? e) (eq? '* (cadr e))))
(define (make-product m1 m2)
  (cond ((=number? m1 0) 0)
        ((=number? m2 0) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))
(define (multiplier e)
  (car e))
(define (multiplicand e)
  (caddr e))
(define (sum? e)
  (and (pair? e) (eq? '+ (cadr e))))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))
(define (addend e)
  (car e))
(define (augend e)
  (caddr e))
(deriv '(x + (3 * (x + (y + 2)))) 'x)
(deriv '(x + (3 * (x + (y + 2)))) 'y)

;; Exercise 2.58b
;; Only(?) possible because deriv evaluates sum condition first.
;; Does not handle nested parentheses.
(define (sum? e)
  (and (pair? e) (member '+ e)))
(define (addend e)
  (define (helper e)
    (if (eq? (car e) '+)
        '()
        (cons (car e) (helper (cdr e)))))
  (if (eq? (cadr e) '+)
      (car e)
      (helper e)))
(define (augend e)
  (let ((rest (cdr (memq '+ e))))
    (if (single? rest)
        (car rest)
        rest)))

(define (multiplicand e)
  (let ((rest (cddr e)))
    (if (single? rest)
        (car rest)
        rest)))

(addend '(x + 3 * y))
(addend '(x * 3 + y))
(augend '(x + 3 * y))
(augend '(x * 3 + y))

(deriv '(x + 2 * 3 * (x + y + 2)) 'x)
(deriv '(x + 2 * 3 * (x + y + 2)) 'y)

;; Exercise 2.59
(define (element-of-set? x set)
  (cond ((null? set) #false)
        ((equal? x (car set)) #true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1)
                                 set2)))
        (else (intersection-set (cdr set1)
                                set2))))

(define (union-set set1 set2)
  (define (helper set1 set2 result)
    (cond ((or (null? set1) (null? set2))
           result)
          ((element-of-set? (car set1) set2)
           (helper (cdr set1) set2 (cons (car set1) result)))
          (else (helper (cdr set1) set2 result))))
  (helper set1 set2 '()))

;; Exercise 2.60
;; multiset

;; element-of? is the exact same O(n)
(define (element-of-multiset? x set)
  (cond ((null? set) #false)
        ((equal? x (car set)) #true)
        (else (element-of-multiset? x (cdr set)))))

;; adjoin-set is O(1)
(define (adjoin-multiset x set)
  (cons x set))

;; union is O(n)
(define (union-multiset set1 set2)
  (append set1 set2))

;; intersect is still O(n^2)
(define (remove x set)
    (define (helper x set result)
      (if (equal? x (car set))
          (append result (cdr set))
          (helper x (cdr set) (cons (car set) result))))
    (helper x set '()))

(define (intersect-multiset set1 set2)
  (define (helper set1 set2 result)
    (cond ((or (null? set1) (null? set2))
           result)
          ((element-of-multiset? (car set1) set2)
           (helper (cdr set1) (remove (car set1) set2) (cons (car set1) result)))
          (else (helper (cdr set1) set2 result))))
  (helper set1 set2 '()))

;; Exercise 2.61
(define (element-of-set? x set)
  (cond ((null? set) #false)
        ((= x (car set)) #true)
        ((< x (car set)) #false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;; Exercise 2.62
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set
                          (cdr set1)
                          set2))
              ((< x2 x1) (intersection-set
                          set1
                          (cdr set2)))))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1 (union-set
                                 (cdr set1)
                                 (cdr set2))))
                      ((< x1 x2)
                       (cons x1 (union-set
                                 (cdr set1)
                                 set2)))
                      ((< x2 x1)
                       (cons x2 (union-set
                                 set1
                                 (cdr set2)))))))))

;; Sets as binary trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #false)
        ((= x (entry set)) #true)
        ((< x (entry set))
         (element-of-set?
          x
          (left-branch set)))
        ((> x (entry set))
         (element-of-set?
          x
          (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))

;; Exercise 2.63
;; Both are correct in that they return elements in sorted order.
;; The second method is O(n). The first could be O(n^2) given an
;; unbalanced tree and O(n) append. If we assume a balanced tree,
;; it would be O(n log n).
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append
       (tree->list-1
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1
              (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))


(define tree1 (list 7
                    (list 3
                          (list 1 '() '())
                          (list 5 '() '()))
                    (list 9
                          '()
                          (list 11 '() '()))))

(tree->list-1 tree1)
(tree->list-2 tree1)

;; Exercise 2.64
;; partial-tree divides the elements into half, recursively calling itself
;; on the first half as the left branch and second half as the right branch.
;; (1 3 5 7 9 11) forms the tree
;;        5
;;      /  \
;;    /     \
;;   1       9
;;   \      / \
;;    \   /    \
;;    3  7     11
;; The time complexity is O(n)
(define (list->tree elements)
  (car (partial-tree
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size
             (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree
                elts left-size)))
          (let ((left-tree
                 (car left-result))
                (non-left-elts
                 (cdr left-result))
                (right-size
                 (- n (+ left-size 1))))
            (let ((this-entry
                   (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree
                     (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

(list->tree '(1 3 5 7 9 11))

;; Exercise 2.65
(define tree->list tree->list-2)
(define (union-set set1 set2)
  (define (union-list list1 list2)
    (cond ((null? list1) list2)
          ((null? list2) list1)
          (else (let ((x (car list1))
                      (y (car list2)))
                  (cond ((= x y)
                         (cons x (union-list (cdr list1) (cdr list2))))
                        ((< x y)
                         (cons x (union-list (cdr list1) list2)))
                        ((> x y)
                         (cons y (union-list list1 (cdr list2)))))))))
  (let ((list1 (tree->list set1))
        (list2 (tree->list set2)))
    (let ((result (union-list list1 list2)))
      (list->tree result))))

(define (intersection-set set1 set2)
  (define (intersect-list list1 list2)
    (cond ((null? list1) '())
          ((null? list2) '())
          (else (let ((x (car list1))
                      (y (car list2)))
                  (cond ((= x y)
                         (cons x (intersect-list (cdr list1) (cdr list2))))
                        ((< x y)
                         (intersect-list (cdr list1) list2))
                        ((> x y)
                         (intersect-list list1 (cdr list2))))))))
  (let ((list1 (tree->list set1))
        (list2 (tree->list set2)))
    (let ((result (intersect-list list1 list2)))
      (list->tree result))))

(tree->list (intersection-set (list->tree '(1 3 4 5)) (list->tree '(1 2 3 6))))
(tree->list (union-set (list->tree '(1 3 4 5)) (list->tree '(1 2 3 6))))

;; Exercise 2.66
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key
                 (key (car set-of-records)))
         (car set-of-records))
        (else
         (lookup given-key
                 (cdr set-of-records)))))

(define (lookup-tree given-key set)
  (cond ((null? set) #nil)
        ((= given-key (key (entry set)))
         (entry set))
        ((> given-key (key (entry set)))
         (lookup-tree given-key (right-branch set)))
        ((< given-key (key (entry set)))
         (lookup-tree given-key (left-branch set)))))

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

