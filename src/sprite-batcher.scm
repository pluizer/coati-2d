(declare (unit sprite-batcher)
         (uses batcher
               blend
               camera
               misc
               primitives
               shader
               sprite))

(import srfi-1
        srfi-4
        matchable)

(define-record sprite-batcher
  batcher
  sprite-ids)

(define-record sprite-batch-id
  batch-id
  matrix
  sprite)


(define (sprite-batcher:create #!optional (shader default-shader))
  (make-sprite-batcher 
   (batcher:create shader
		   *triangle-rect-mode*
		   4)
   (list)))

(define (sprite-batcher:push! sprite-batcher sprite matrix #!optional colour)
  (let* ((sprite-id (make-sprite-batch-id 
		     (batcher:push!
		      (sprite-batcher-batcher sprite-batcher)
		      ;; Vertex data
		      (sprite:vertex-data sprite matrix)
		      ;; Coord data
		      (sprite:coord-data sprite)
		      ;; Colour (white)
		      (or colour
			  (make-f32vector 16 1)))
		     matrix sprite)))
    (sprite-batcher-sprite-ids-set! 
     sprite-batcher 
     (cons sprite-id (sprite-batcher-sprite-ids sprite-batcher)))
    sprite-id))


(define (sprite-batcher:change! sprite-batcher s-b-id matrix
				#!optional colour)
  (sprite-batch-id-matrix-set! s-b-id matrix)
  (match-let ((($ sprite-batch-id batch-id matrix sprite) s-b-id))
	     (if colour
	      (batcher:change! (sprite-batcher-batcher sprite-batcher) 
			       batch-id
			       ;; Vertex data
			       vertex: (sprite:vertex-data sprite matrix)
			       coord:  (sprite:coord-data sprite)
			       colour: colour)
	      (batcher:change! (sprite-batcher-batcher sprite-batcher) 
			       batch-id
			       ;; Vertex data
			       vertex: (sprite:vertex-data sprite matrix)
			       coord:  (sprite:coord-data sprite)
			       ))))

(define (sprite-batcher:update! sprite-batcher)
  (for-each
   (lambda (sprite-id)
     (match-let ((($ sprite-batch-id batch-id matrix sprite)
		  sprite-id))
		;; TODO polls sprites too much.
		(when (sprite:animated? sprite)
		 (batcher:change! (sprite-batcher-batcher sprite-batcher)
				  batch-id
				  coord: (sprite:coord-data sprite)))))
   (sprite-batcher-sprite-ids sprite-batcher)))

(define (sprite-batcher:remove! sprite-batcher id)
  (batcher:remove! (sprite-batcher-batcher sprite-batcher)
		   (sprite-batch-id-batch-id id))
  (sprite-batcher-sprite-ids-set! sprite-batcher
   (remove (=? id) (sprite-batcher-sprite-ids sprite-batcher))))

(define (sprite-batcher:clear! sprite-batcher)
  (batcher:clear! (sprite-batcher-batcher sprite-batcher))
  (sprite-batcher-sprite-ids-set! sprite-batcher (list)))

;; Render with matrices instead of camera
(define (sprite-batcher:render* sprite-batcher projection view)
  (when (not (null? (sprite-batcher-sprite-ids sprite-batcher)))
    (batcher:render (sprite-batcher-batcher sprite-batcher) 
                    projection (if %target-is-screen?
                                   view
                                   ;; Flip the y-axis of all framebuffer targets.
                                   (matrix:scale (vect:create 1 -1) view))
                    (%current-colour))))

(define (sprite-batcher:render sprite-batcher)
  (sprite-batcher:render* sprite-batcher
                          (camera:projection (current-camera))
                          (camera:view (current-camera))))
;; %
