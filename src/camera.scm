(declare (unit camera)
         (uses primitives
               window))

(define-record camera
  pos width height projection view)

(define (camera:create pos #!optional (size (window:size)))
  (make-camera pos (vect:x size) (vect:y size)
               (ortho-matrix (vect:x size)
                             (vect:y size))
               (look-at-matrix pos pos 1)))

(define camera:pos camera-pos)
(define camera:width camera-width)
(define camera:height camera-height)
(define camera:projection camera-projection)
(define camera:view camera-view)

(define (%gui-camera)
  (let ((size (window:size)))
    (camera:create (vect:create (/ (vect:x size) 2) (/ (vect:y size) 2)))))

(define %camera-stack (list))
(define (current-camera) (if (null? %camera-stack)
                             ;; Default current camera
                             (%gui-camera)
                             (car %camera-stack)))


(define (camera:snap pos world-size)
  (let* ((win (window:size))
         (sx (/ (vect:x win) (vect:x world-size)))
         (sy (/ (vect:y win) (vect:y world-size))))
    (vect:create (/ (round (* (vect:x pos) sx)) sx)
                 (/ (round (* (vect:y pos) sy)) sy))))

(define (with-camera/proc camera thunk)
  (set! %camera-stack (cons camera %camera-stack))
  (thunk)
  (set! %camera-stack (cdr %camera-stack)))
