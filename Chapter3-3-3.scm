;; Exercise 3.24
(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) #false)
          ((same-key? key (caar records))
           (car records))
          (else (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            #false)))
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value)
                            (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation:
                          TABLE" m))))
    dispatch))

(define (close-enough? x y) (< (abs (- x y)) 1e-3))
(define tb-nums (make-table close-enough?))
((tb-nums 'insert-proc!) 3.0 'a)
((tb-nums 'lookup-proc) 3.1) ;; #f
((tb-nums 'lookup-proc) 3.0001) ;; a

;; Exercise 3.25
;; Generalizing by recursively calling into subtables.
;; This solution is not fully correct since it does not handle
;; list of keys that are prefixes of another list of keys.
;; For example:
;; (insert '(a b c) 3) followed by (insert '(a b) 2)
;; would wipe out the subtable created by the first insert call.
;; To resolve this, we would need to represent a subtable from
;; a value differently. Perhaps by forming a pair between (key . single)
;; for a single value. Not implemented here.
(define (make-table-multikey)
  (define (assoc key records)
    (cond ((null? records) #false)
          ((equal? key (caar records))
           (car records))
          (else (assoc key (cdr records)))))

  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (define (helper table keys)
        (if (null? (cdr keys))
            (let ((record (assoc (car keys) (cdr table))))
              (if record
                  (cdr record)
                  #false))
            (let ((subtable (assoc (car keys) (cdr table))))
              (if subtable
                  (helper subtable (cdr keys))
                  #false))))
      (helper local-table keys))
    (define (insert! keys value)
      (define (helper table keys)
        (if (null? (cdr keys))
            (let ((record
                   (assoc (car keys) (cdr table))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! table (cons (cons (car keys) value)
                                        (cdr table)))))
            (let ((subtable
                   (assoc (car keys) (cdr table))))
              (if subtable
                  (helper subtable (cdr keys))
                  (begin (set-cdr!
                          table
                          (cons (list (car keys))
                                (cdr table)))
                         (helper (cadr table) (cdr keys))))))
        'ok)
      (helper local-table keys))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation:
                          TABLE" m))))
    dispatch))

(define tb (make-table-multikey))
((tb 'insert-proc!) '(a b c) 3.14)
((tb 'lookup-proc) '(a b c))
((tb 'insert-proc!) '(d e f g h) 2.718)
((tb 'lookup-proc) '(d e f g h))

;; Exercise 3.26
;; Assuming that we already know how to represent a list
;; as a binary tree, we can represent a table as a binary tree
;; where each node can be a subtable or a value.

;; Exercise 3.27
;; Basically, there is an environment which contains the table
;; which is referenced in every call to memo-fib.
;; Pre-computed values get set in the table and retrieved in constant time.
;; (memoize fib) does not work because the call to fib in the line 
;; (let ((result (f x))) is not memoized.

