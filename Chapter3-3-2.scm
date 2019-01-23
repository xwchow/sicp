(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) 
  (set-car! queue item))
(define (set-rear-ptr! queue item) 
  (set-cdr! queue item))

(define (empty-queue? queue) 
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an 
              empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else (set-cdr! (rear-ptr queue) 
                          new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with 
                 an empty queue" queue))
        (else (set-front-ptr! 
               queue 
               (cdr (front-ptr queue)))
              queue)))

;; Exercise 3.21
;; The issue basically an implementation detail whereby the tail ptr
;; is not dereferenced when the queue is emptied.
(define (print-queue q)
  (display (front-ptr q)))

(define q1 (make-queue))
(insert-queue! q1 'a) ;; ((a) a)
(insert-queue! q1 'b) ;; ((a b) b)
(delete-queue! q1) ;; ((b) b)
(delete-queue! q1) ;; (() b)
(print-queue q1)

;; Exercise 3.22
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an 
              empty queue" front-ptr)
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else (set-cdr! rear-ptr new-pair)
                    (set-rear-ptr! new-pair)))))

    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with 
                 an empty queue" front-ptr))
            (else (set-front-ptr! (cdr front-ptr)))))
    
    (define (print-queue) (display front-ptr))

    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'front-queue) (front-queue))
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))
            ((eq? m 'print-queue) (print-queue))
            (else (error "Undefined operation:" m))))
    dispatch))

(define (empty-queue? q) (q 'empty-queue?))
(define (front-queue? q) (q 'front-queue))
(define (print-queue q) (q 'print-queue) (newline))
(define (insert-queue! q item)
  ((q 'insert-queue!) item)
  q)
(define (delete-queue! q)
  (q 'delete-queue!)
  q)

(define q1 (make-queue))
(insert-queue! q1 'a) ;; ((a) a)
(insert-queue! q1 'b) ;; ((a b) b)
(delete-queue! q1) ;; ((b) b)
(delete-queue! q1) ;; (() b)
(print-queue q1)

;; Exercise 3.23
;; Define a node that remembers its parent link i.e
;; we need to represent a doubly-linked list.
(define (make-node data front back) (list data front back))
(define (get-data node) (car node))
(define (get-front node) (cadr node))
(define (get-back node) (caddr node))
(define (set-front! node front) (set-car! (cdr node) front))
(define (set-back! node back) (set-cdr! (cdr node) back))

(define (make-deque) (cons '() '()))
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))
(define (empty-deque? deque)
  (or (null? (front-ptr deque))
      (null? (rear-ptr deque))))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "Empty deque")
      (get-data (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "Empty deque")
      (get-data (rear-ptr deque))))

(define (print-deque deque)
  (define (loop node)
    (display " ")
    (display (get-data node))
    (if (not (null? (get-front node)))
        (loop (get-front node))))
  (cond ((empty-deque? deque)
         (display "Deque is currently empty.")
         (newline))
        (else (display "Front of deque: ")
              (display (front-deque deque)) (newline)
              (display "Rear of deque: ")
              (display (rear-deque deque)) (newline)
              (display "Deque:")
              (loop (front-ptr deque))
              (newline))))

(define (front-insert-deque! deque item)
  (let ((new-node (make-node item #nil #nil)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node)
           deque)
          (else
           (set-back! (front-ptr deque) new-node)
           (set-front! new-node (front-ptr deque))
           (set-front-ptr! deque new-node)
           deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE! called with 
                 an empty deque" deque))
        (else (set-front-ptr! 
               deque 
               (get-front (front-ptr deque))))))

(define (rear-insert-deque! deque item)
  (let ((new-node (make-node item #nil #nil)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node)
           deque)
          (else
           (set-front! (rear-ptr deque) new-node)
           (set-back! new-node (rear-ptr deque))
           (set-rear-ptr! deque new-node)
           deque))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE! called with 
                 an empty deque" deque))
        (else (set-rear-ptr! 
               deque 
               (get-back (rear-ptr deque))))))

(define deque1 (make-deque))
(print-deque deque1)
(front-insert-deque! deque1 'a)
(print-deque deque1)
(front-insert-deque! deque1 'b)
(print-deque deque1)
(front-insert-deque! deque1 'c)
(print-deque deque1)
(front-delete-deque! deque1)
(print-deque deque1)
(front-delete-deque! deque1)
(print-deque deque1)
(rear-insert-deque! deque1 'd)
(print-deque deque1)
(front-delete-deque! deque1)
(print-deque deque1)
(front-delete-deque! deque1)
(print-deque deque1)
(front-insert-deque! deque1 'e)
(print-deque deque1)
(rear-delete-deque! deque1)
(print-deque deque1)
