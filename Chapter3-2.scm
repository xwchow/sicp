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
;; Done. Not here.
