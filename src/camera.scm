(declare (unit camera)
         (uses primitives
               window))

(define-record camera
  pos width height projection view)

(define (camera:create pos #!optional (size (window:size)))
  (make-camera pos (vect:x size) (vect:y size)
                (ortho-matrix (vect:x size) (vect:y size))
                (look-at-matrix pos pos 1)))

(define camera:pos camera-pos)
(define camera:width camera-width)
(define camera:height camera-height)
(define camera:projection camera-projection)
(define camera:view camera-view)

(define (%gui-camera)
  (let* ((size (window:size))
         (w (vect:x size))
         (h (vect:y size))
         (cam (camera:create (vect:create (/ w 2) (/ h 2)))))
    cam))

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

(define (camera:screen->world cam win-size screen-pos)
  (vect:create
    (+ (vect:x (camera:pos cam))
       (* (- (vect:x screen-pos) (* 0.5 (vect:x win-size)))
          (/ (camera:width cam) (vect:x win-size))))
    (+ (vect:y (camera:pos cam))
       (* (- (vect:y screen-pos) (* 0.5 (vect:y win-size)))
          (/ (camera:height cam) (vect:y win-size))))))

(define (camera:world->screen cam win-size world-pos)
  (vect:create
    (+ (* 0.5 (vect:x win-size))
       (* (- (vect:x world-pos) (vect:x (camera:pos cam)))
          (/ (vect:x win-size) (camera:width cam))))
    (+ (* 0.5 (vect:y win-size))
       (* (- (vect:y world-pos) (vect:y (camera:pos cam)))
          (/ (vect:y win-size) (camera:height cam))))))

(define (camera:screen->world/viewport cam viewport-origin viewport-size screen-pos)
  (camera:screen->world cam viewport-size (vect- screen-pos viewport-origin)))

(define (with-camera/proc camera thunk)
  (set! %camera-stack (cons camera %camera-stack))
  (thunk)
  (set! %camera-stack (cdr %camera-stack)))
