(declare (unit trans)
	 (uses primitives
	       sprite))

(use gl-math)

(define-record trans
  position
  origin
  rotation
  scale
  flip-v?
  flip-h?)

(define-record trans-change
  position
  origin
  rotation
  scale
  flip-v?
  flip-h?)

(define %not-set (gensym 'not-set))

(define (trans:create position
		      #!key
		      origin
		      rotation
		      scale
		      flip-v?
		      flip-h?)
  (make-trans position
	      (or origin (zero-vect))
	      (or rotation 0.0)
	      (or scale (vect:create 1 1))
	      flip-v?
	      flip-h?))

;; Returns a new trans-change object.
;; A trans-change is used to change only certain slots of a trans. Create
;; a trans-change by calling this funtion with the key names + new value
;; of the slot you want to change in a trans.
;; Calling ''trans-change->trans'' with the old trans will result in a new
;; trans where or key names givin are replaced with the new value.
(define (trans-change:create #!key
			     position
			     origin
			     rotation
			     scale
			     (flip-v? %not-set)
			     (flip-h? %not-set))
  (make-trans-change position origin rotation scale flip-h? flip-v?))

;; Returns a new trans from an old one using the slots specified in the given
;; trans-change.
(define (trans-change->trans old-trans trans-change)
  (make-trans (or (trans-change-position trans-change)
		  (trans-position old-trans))
	      (or (trans-change-origin trans-change)
		  (trans-origin old-trans))
	      (or (trans-change-rotation trans-change)
		  (trans-rotation old-trans))
	      (or (trans-change-scale trans-change)
		  (trans-scale old-trans))
	      (if (eq? (trans-change-flip-v? trans-change) %not-set)
		  (trans-flip-v? old-trans)
		  (trans-change-flip-v? trans-change))
	      (if (eq? (trans-change-flip-h? trans-change) %not-set)
		  (trans-flip-h? old-trans)
		  (trans-change-flip-h? trans-change))))

(define trans:position trans-position)
(define trans:origin   trans-origin)
(define trans:rotation trans-rotation)
(define trans:scale    trans-scale)
(define trans:flip-v?  trans-flip-v?)
(define trans:flip-h?  trans-flip-h?)

(define (trans->matrix trans)
  (let* ((pos (trans-position trans))
	 (origin (trans-origin trans)))
    (matrix:translate
     pos
     (matrix:scale (trans-scale trans)
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
				       ))))))))
