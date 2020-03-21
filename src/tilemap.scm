(declare (unit tilemap)
         (uses primitives
               shader
               sprite-batcher
               texture
               misc))

(include "helpers.scm")

(import srfi-1
        srfi-4
        (prefix epoxy gl::)
        (prefix gl-math gl::))


(define (tilemap:create #!key new-coords-callback (shader default-shader))
  (let ((batcher (sprite-batcher:create shader))
	;; Rememer the last added coordinate and the width and height
	;; so that the sprite-batch does not have to be repopulated
	;; when these values haven't changed.
	(changed? (make-change-check))
	;; Cache all active coords.
	(active-coords (list)))
    (let ((raw
	   (lambda (coord
		    width height
		    ;; A function that takes a coordinate and returns a tile number.
		    tile-func
                    projection view)
	     ;; If ``coord`` ``width`` ``height`` or ``tile-func`` 
	     ;; changed we'll repopulate te sprite-batch.
	     (when (changed? coord width height tile-func)
	       (let ( ;; List of all coordinates
		     (coords
		      (map (lambda (x)
			     (coord:create (+ (modulo x width)
					      (coord:x coord))
					   (+ (floor (/ x width))
					      (coord:y coord))))
			   (iota (* width height)))))
		 ;; Check which coords will be newly added and which are the
		 ;; ones too keep
		 (let-values (((keep new)
			       (partition (lambda (x)
					    (member x active-coords)) coords)))
		   (when (or (not (null? new))
			     (not (= (length active-coords)
				     (length coords))))
		     ;; Call the optional callback with the coords to be removed
		     ;; and the coords that are being added.
 		     (when new-coords-callback
		       (new-coords-callback
                        coord 
                        (filter (lambda (x) (not (member x coords))) active-coords)
                        new))
		     ;; Clear the previously added sprites and add the new ones
		     ;; (Dumbly clearing everything an reading is often 
		     ;; faster than keeping track of and deleting all unneeded
		     ;; handles one by one.)
		     (sprite-batcher:clear! batcher)
		     (for-each
		      (lambda (tile-coord)
			(let ((sprite (tile-func tile-coord)))
			  ;; It is possible not to have a sprite at these coords.
			  (when sprite
			    ;; Push the tile to the batcher.
			    (sprite-batcher:push! batcher sprite
						  (trans->matrix
						   (trans:create
						    (vect:create
						     (- (coord:x tile-coord) 
							(coord:x coord))
						     (- (coord:y tile-coord)
							(coord:y coord)))))))))
		      coords)
		     (set! active-coords coords)))))
	     ;; Render the sprite-batch
	     (sprite-batcher:render* batcher projection view))))
      ;; Function returned by ``tilemap:create``. Renders the map from
      ;; the ``top-left`` coordinate. (which is a vect not a coord so
      ;; fractions are possible).
      (lambda (top-left width height tile-func projection view trans-func)
	(sprite-batcher:update! batcher)
	(let* ((x (vect:x top-left))
	       (y (vect:y top-left))
	       (fx (floor x))
	       (fy (floor y))
	       (rx (- x fx 1))
	       (ry (- y fy 1)))
	  
	  (raw (coord:create (- (- fx) 1) (- (- fy) 1))
	       (+ width 1) (+ height 1)
	       tile-func
	       projection
	       (maybe trans-func (matrix:translate (vect:create rx ry) view))))))))

;; Renders a procedural generated tilemap from the position of a camera.
(define (tilemap:render tilemap width height tile-func #!optional trans-func)
  (let ((projection (camera:projection (current-camera)))
        (view (camera:view (current-camera))))
   (tilemap (camera:pos (current-camera)) width height tile-func projection view trans-func)))
