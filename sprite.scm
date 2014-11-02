(declare (unit sprite)
	 (uses texture))

(use 2d-primitives)

(define-record frame
  rectangle)

(define (frame:create texture rectangle)
  (make-frame
   ;; Map texels to pixels.
   (let* ((w (vect:x (texture:size texture)))
	  (h (vect:y (texture:size texture)))
	  (r (rect:create (+ (rect:l rectangle)
			     (/ .5 w))
			  (- (rect:r rectangle)
			     (/ .5 w))
			  (+ (rect:t rectangle)
			     (/ .5 h))
			  (- (rect:b rectangle)
			     (/ .5 h)))))
     (f32vector (rect:l r) (rect:t r)
		(rect:l r) (rect:b r)
		(rect:r r) (rect:b r)
		(rect:r r) (rect:t r)))))

(define-record animation
  frames
  interval
  epoch)

(define (animation:create interval frames)
  (make-animation frames interval (current-milliseconds)))


(define (animation:new-frame? animation)
  (> (- (current-milliseconds)
	(animation-epoch animation))
     (animation-interval animation)))

(define (animation:frame animation)
  (when (animation:new-frame? animation)
	(let* ((frames (animation-frames animation))
	       (ret (car frames)))
	  (animation-frames-set! animation (append (cdr frames) (list ret)))
	  ret)
	(animation-epoch-set! animation (current-milliseconds)))
  (car (animation-frames animation)))

(define-record sprite
  animation frame)

(define (sprite:create texture rectangles 
		       #!optional (interval (/ 1000 20)))
  (let ((l (length rectangles)))
    (cond ((zero? l)
	  (error "need at least one rectangle"))
	 ((= l 1)
	  (make-sprite #f (frame:create texture (car rectangles))))
	 (else 
	  (make-sprite (animation:create
			interval
			(map (lambda (rectangle)
			       (frame:create texture rectangle))
			     rectangles))
		       #f)))))

;; Is this sprite an animation?
(define (sprite:animated? sprite)
  (not (sprite-frame sprite)))

(define (sprite:new-frame? sprite)
  (if (sprite-animation sprite)
      (animation:new-frame? (sprite-animation sprite))
      #f))

(define (sprite:frame sprite)
  (if (sprite-animation sprite)
      (animation:frame (sprite-animation sprite))
      (sprite-frame sprite)))

(define sprite:rectangle 
  (o frame-rectangle sprite:frame))
