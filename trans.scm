(declare (unit trans)
	 (uses primitives
	       sprite))

(define-record trans
  position 
  origin
  rotation
  flip-v?
  flip-h?)

(define (trans:create position
		      #!key
		      origin
		      rotation
		      flip-v?
		      flip-h?)
  (make-trans position
	      (or origin (zero-vect))
	      (or rotation 0.0)
	      flip-v?
	      flip-h?))

(define (trans:vertex-data trans)
  (polygon+
   (polygon:rotate(rect->polygon (rect:create 0 1 0 1))
		  (trans:rotation trans)
		  (trans:origin trans))
   (trans:position trans)))

(define (trans:coord-data trans sprite)
  (let* ((rect (sprite:rectangle sprite))
	 (h (trans:flip-h? trans))
	 (v (trans:flip-v? trans))
	 (r (rect:create ((if h rect:r rect:l) rect)
			 ((if h rect:l rect:r) rect)
			 ((if v rect:t rect:b) rect)
			 ((if v rect:b rect:t) rect))))
    (f32vector (rect:l r) (rect:b r)
	       (rect:l r) (rect:t r)
	       (rect:r r) (rect:t r)
	       (rect:r r) (rect:b r))))

(define trans:position trans-position)
(define trans:origin   trans-origin)
(define trans:rotation trans-rotation)
(define trans:flip-v?  trans-flip-v?)
(define trans:flip-h?  trans-flip-h?)
