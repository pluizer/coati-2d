(declare (unit sprite)
	 (uses texture))

(use 2d-primitives)

(define-record frame
  rectangle)

(define (%frame:create texture rectangle)
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
  frames	;; all the animation's frames.
  interval	;; number of ms before frame must update.
  epoch		;; last time (in ms) frame was updated.
  )

(define (%animation:create interval frames)
  (make-animation frames interval (current-milliseconds)))


(define (%animation:new-frame? animation)
  (> (- (current-milliseconds)
	(animation-epoch animation))
     (animation-interval animation)))

(define (%animation:frame animation)
  (when (%animation:new-frame? animation)
	(let* ((frames (animation-frames animation))
	       (ret (car frames)))
	  (animation-frames-set! animation (append (cdr frames) (list ret)))
	  ret)
	(animation-epoch-set! animation (current-milliseconds)))
  (car (animation-frames animation)))

(define-record sprite
  ;; when the sprite has more than one frame, ''animation'' will contain
  ;; an animation record and ''frame'' will be #f.
  ;; else ''frame'' will contain a frame record and ''animation'' will
  ;; be #f.
  animation
  frame
  )

;; Create a new sprite with a list of rectangles that describe the
;; the sprite's frames on the texture.
;; Frames will updated every ''interval'' milliseconds.
(define (sprite:create texture rectangles 
		       #!optional (interval (/ 1000 20)))
  (let ((l (length rectangles)))
    (cond ((zero? l)
	  (error "need at least one rectangle"))
	 ((= l 1)
	  (make-sprite #f (%frame:create texture (car rectangles))))
	 (else 
	  (make-sprite (%animation:create
			interval
			(map (lambda (rectangle)
			       (%frame:create texture rectangle))
			     rectangles))
		       #f)))))

;; Is this sprite an animation?
(define (sprite:animated? sprite)
  (not (sprite-frame sprite)))

;; Is it time to update the current frame of the sprite?
(define (sprite:new-frame? sprite)
  (if (sprite:animated? sprite)
      (%animation:new-frame? (sprite-animation sprite))
      #f))

;; Give the current frame of the sprite.
(define (sprite:frame sprite)
  (if (sprite:animated? sprite)
      (%animation:frame (sprite-animation sprite))
      (sprite-frame sprite)))

;; Give the rectangle that describes the current frame on the texture.
(define sprite:rectangle 
  (o frame-rectangle sprite:frame))
