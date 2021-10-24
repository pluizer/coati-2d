(declare (unit static-tilemap)
         (uses primitives
               shader
               sprite-batcher
               texture
               misc))

(import srfi-1
        srfi-4
        matchable
        (prefix epoxy gl::)
        (prefix gl-math gl::))

(define-record static-tilemap
  width height tile-size isometric? sprites sprite-batcher)

(define (static-tilemap:create width height tile-size isometric?
			       #!optional (shader default-shader))
  (make-static-tilemap width height tile-size isometric?
		       (make-vector (* width height) #f)
		       (sprite-batcher:create shader)))

(define (static-tilemap:set! static-tilemap coord sprite)
  (let ((x (coord:x coord))
	(y (coord:y coord)))
    (match static-tilemap
	   (($ 'static-tilemap width height tile-size isometric? sprites sprite-batcher)
	    (let* ((w (coord:x tile-size))
		   (h (coord:y tile-size))
		   (pos (+ (* width y) x))
		   (x (* w x))
		   (y (* h y))
		   (pos (+ (* width y) x))
		   (id (vector-ref sprites pos))
		   (trans (trans->matrix
			   (trans:create
			    (if isometric?
				(vect:create (+ (* x -.5)
						(* y .5))
					     (+ (* x -.25)
						(* y -.25)))
				(vect:create x y))))))
	      (when id (sprite-batcher:remove! sprite-batcher id))
	      (vector-set! sprites pos
			   ;; Push the tile to the batcher.
			   (match sprite
				  ;; sprite with no special colour
				  ((? sprite? sprite)
				   (sprite-batcher:push! sprite-batcher sprite trans))
				  ((sprite: sprite colour: colour)
				   (sprite-batcher:push! sprite-batcher sprite trans colour)))
			   ))))))

(define (static-tilemap:clear! static-tilemap)
  (sprite-batcher:clear! (static-tilemap-sprite-batcher static-tilemap)))

(define (static-tilemap:render static-tilemap)
  (sprite-batcher:render (static-tilemap-sprite-batcher static-tilemap)))
