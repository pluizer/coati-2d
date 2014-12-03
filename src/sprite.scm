(declare (unit sprite)
	 (uses primitives)
	 (uses texture))

(define-record frame
  rectangle)

(define (%frame:create texture rectangle)
  (make-frame
   ;; Map texels to pixels.
   (let ((w (vect:x (texture:size texture)))
	 (h (vect:y (texture:size texture))))
     
     (let ((r
	    (rect:create (+ (rect:l rectangle)
			    (/ .5 w))
			 (- (rect:r rectangle)
			    (/ .5 w))
			 (+ (rect:t rectangle)
			    (/ .5 h))
			 (- (rect:b rectangle)
			    (/ .5 h)))))
       r))))

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
  size
  )

;; Create a new sprite with a list of rectangles that describe the
;; the sprite's frames on the texture.
;; Frames will updated every ''interval'' milliseconds.
(define (sprite:create texture rectangles 
		       #!optional
		       (size (vect:create 1 1))
		       (interval (/ 1000 20)))
  (let ((l (length rectangles)))
    (cond ((zero? l)
	  (error "need at least one rectangle"))
	 ((= l 1)
	  (make-sprite #f (%frame:create texture (car rectangles)) size))
	 (else 
	  (make-sprite (%animation:create
			interval
			(map (lambda (rectangle)
			       (%frame:create texture rectangle))
			     rectangles))
		       #f
		       size)))))

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


(define (sprite:vertex-data sprite matrix)
  (apply f32vector (flatten
    (map (lambda (vect)
	   (let ((r (vect*matrix vect matrix)))
	     (list (vect:x r)
		   (vect:y r))))
	 (let* ((size (sprite-size sprite))
		(w (vect:x size))
		(h (vect:y size)))
	   (polygon->vects (rect->polygon
			    (rect:create 0 w 0 h))))))))

(define (sprite:coord-data sprite)
  (let ((rect (sprite:rectangle sprite)))
    (f32vector (rect:l rect) (rect:b rect)
	       (rect:l rect) (rect:t rect)
	       (rect:r rect) (rect:t rect)
	       (rect:r rect) (rect:b rect))))

;; Create a sprite not by specifying the coordinates by rectangles,
;; but by indices.
;; |-w-|
;; +---+---+---+ -
;; |   |   |   | |
;; | 0 | 1 | 2 | h
;; |   |   |   | |
;; +---+---+---+ -
;; |   |   |   |
;; | 3 | 4 | 5 |
;; |   |   |   |
;; +---+---+---+
(define (sprite:create-from-indices texture tiles-w tiles-h indices
				    #!optional
				    (size (vect:create 1 1))
				    (interval (/ 1000 20)))
  (let ((w (/ 1 tiles-w))
	(h (/ 1 tiles-h)))
    (sprite:create texture
     (map (lambda (coord)
	    (let ((x (vect:x coord))
		  (y (vect:y coord)))
	      (rect:create (* x w) (+ (* x w) w)
			   (+ (* y h) h) (* y h))))
	  (map (lambda (index)
		 (vect:create (modulo index tiles-w)
			      (floor (/ index tiles-w))))
	       indices))
     size
     interval)))

(define sprite:size sprite-size)
