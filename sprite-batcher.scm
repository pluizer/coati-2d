(declare (unit sprite-batcher)
	 (uses batcher
	       primitives
	       shader
	       sprite
	       trans))

(use srfi-1
     srfi-4
     matchable)

(define-record sprite-batcher
  batcher
  sprite-ids)

(define-record sprite-batch-id
  batch-id
  trans
  sprite)

(define (sprite-batcher:create)
  (make-sprite-batcher 
   (batcher:create default-shader
		   *triangle-rect-mode*
		   4)
   (list)))

(define (sprite-batcher:push! sprite-batcher sprite trans)
  (let* ((sprite-id (make-sprite-batch-id 
		     (batcher:push!
		      (sprite-batcher-batcher sprite-batcher)
		      ;; Vertex data
		      (trans:vertex-data trans)
		      ;; Coord data
		      (trans:coord-data trans sprite)
		      ;; Colour (white)
		      (make-f32vector 16 1 #t))
		     trans sprite)))
    (sprite-batcher-sprite-ids-set! 
     sprite-batcher 
     (cons sprite-id (sprite-batcher-sprite-ids sprite-batcher)))
    sprite-id))


(define (sprite-batcher:change! sprite-batcher sprite-batch-id trans)
  (sprite-batch-id-trans-set! sprite-batch-id trans)
  (match-let ((($ sprite-batch-id batch-id trans sprite) sprite-batch-id))
	     (batcher:change! (sprite-batcher-batcher sprite-batcher) 
			      batch-id
			      ;; Vertex data
			      vertex: (trans:vertex-data trans)
			      coord:  (trans:coord-data trans sprite))))

(define (sprite-batcher:update! sprite-batcher)
  (for-each
   (lambda (sprite-id)
     (match-let ((($ sprite-batch-id batch-id trans sprite)
		  sprite-id))
		;; TODO polls sprites too much.
		(when (sprite:animated? sprite)
		 (batcher:change! (sprite-batcher-batcher sprite-batcher)
				  batch-id
				  coord: (trans:coord-data trans sprite)))))
   (sprite-batcher-sprite-ids sprite-batcher)))

(define (sprite-batcher:remove! sprite-batcher id)
  (batcher:remove! (sprite-batcher-batcher sprite-batcher) id)
  (sprite-batcher-sprite-ids-set! sprite-batcher
   (remove (lambda (x) (= (car x) id))
	   (sprite-batcher-sprite-ids sprite-batcher))))

(define (sprite-batcher:clear! sprite-batcher)
  (batcher:clear! (sprite-batcher-batcher sprite-batcher))
  (sprite-batcher-sprite-ids-set! sprite-batcher (list)))

(define (sprite-batcher:render sprite-batcher projection view)
  (batcher:render (sprite-batcher-batcher sprite-batcher) 
		  projection view))
