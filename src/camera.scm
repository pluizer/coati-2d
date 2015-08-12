(declare (unit camera)
         (uses primitives
               window))

(define-record camera
  pos zoom width height projection view)

(define (camera:create pos zoom #!optional (width 1) (height 1))
  (make-camera pos zoom width height
               (ortho-matrix (* zoom width)
                             (* zoom height))
               (look-at-matrix pos pos 1)))

(define camera:pos camera-pos)
(define camera:zoom camera-zoom)
(define camera:width camera-width)
(define camera:height camera-height)
(define camera:projection camera-projection)
(define camera:view camera-view)

;; TODO FIXME, change when resolution changes.
(define %camera-stack (list (camera:create (zero-vect) 1 1 1)))
(define (current-camera) (car %camera-stack))


(define (with-camera/proc camera thunk)
  (set! %camera-stack (cons camera %camera-stack))
  (set! %current-camera camera)
  (thunk)
  (set! %camera-stack (cdr %camera-stack))
  (set! %current-camera (car %camera-stack)))
