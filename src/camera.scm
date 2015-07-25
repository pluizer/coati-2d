(declare (unit camera)
         (uses primitives
               window))

(define-record camera
  pos zoom width height projection view)

(define (camera:create pos zoom #!optional width height)
  (let* ((width  (or width  (vect:x (window:size))))
         (height (or height (vect:y (window:size)))))
   (make-camera pos zoom width height
                (ortho-matrix (* zoom width)
                              (* zoom height))
                (look-at-matrix pos pos 1))))

(define camera:pos camera-pos)
(define camera:zoom camera-zoom)
(define camera:height camera-height)
(define camera:projection camera-projection)
(define camera:view camera-view)
