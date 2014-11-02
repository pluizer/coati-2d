(declare (unit trans)
	 (uses primitives))

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

(define trans:position trans-position)
(define trans:origin   trans-origin)
(define trans:rotation trans-rotation)
(define trans:flip-v?  trans-flip-v?)
(define trans:flip-h?  trans-flip-h?)
