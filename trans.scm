(declare (unit trans))

(use 2d-primitives)

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

(define trans:position trans-position)
(define trans:origin   trans-origin)
(define trans:rotation trans-rotation)
(define trans:flip-v?  trans-flip-v?)
(define trans:flip-h?  trans-flip-h?)
