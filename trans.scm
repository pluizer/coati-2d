(declare (unit trans)
	 (uses primitives
	       sprite))

(use gl-math)

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

(define (trans->matrix trans)
  (let* ((pos (trans-position trans))
	 (origin (trans-origin trans)))
    (matrix:translate
     pos
     (matrix:translate (vect:flip (trans-flip-v? trans)
				  (trans-flip-h? trans)
				  origin)
		       (matrix:rotate
			(trans-rotation trans)
			(matrix:translate
			 (vect- (vect:flip (trans-flip-v? trans)
					   (trans-flip-h? trans)
					   origin))
			 (matrix:flip (trans-flip-v? trans)
				      (trans-flip-h? trans)
				      (identity-matrix)
				      )))))))
