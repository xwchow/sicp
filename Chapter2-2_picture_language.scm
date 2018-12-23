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
