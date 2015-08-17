(declare (unit camera)
         (uses primitives
               window))

(define-record camera
  pos zoom width height projection view)

(define (camera:create pos zoom #!optional (size (window:size)))
  (make-camera pos zoom (vect:x size) (vect:y size)
               (ortho-matrix (* zoom (vect:x size))
                             (* zoom (vect:y size)))
               (look-at-matrix pos pos 1)))

(define camera:pos camera-pos)
(define camera:zoom camera-zoom)
(define camera:width camera-width)
(define camera:height camera-height)
(define camera:projection camera-projection)
(define camera:view camera-view)

(define %camera-stack (list))
(define (current-camera) (if (null? %camera-stack)
                             ;; Default current camera
                             (camera:create (zero-vect) 1)
                             (car %camera-stack)))


(define (with-camera/proc camera thunk)
  (set! %camera-stack (cons camera %camera-stack))
  (thunk)
  (set! %camera-stack (cdr %camera-stack)))
