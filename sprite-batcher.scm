(declare (unit sprite-batcher)
	 (uses batcher
	       shader
	       sprite
	       trans))

(use 2d-primitives
     srfi-1
     srfi-4)

(define-record sprite-batcher
  batcher
  batch-id+sprite)

(define (sprite-batcher:create)
  (make-sprite-batcher 
   (batcher:create default-shader
		   *triangle-rect-mode*
		   4)
   (list)))

(define (sprite-batcher:push! sprite-batcher sprite trans)
  (let* ((id (batcher:push!
	      (sprite-batcher-batcher sprite-batcher)
	      ;; Vertex data
	      (polygon+
	       (polygon:rotate(rect->polygon (rect:create 0 1 0 1))
			      (trans:rotation trans)
			      (trans:origin trans))
	       (trans:position trans))
	      ;; Coord data
	      (sprite:rectangle sprite)
	      ;; Colour (white)
	      (make-f32vector 16 1 #t))))
    (sprite-batcher-batch-id+sprite-set! 
     sprite-batcher
     (cons (cons id sprite)		 
	   (sprite-batcher-batch-id+sprite sprite-batcher)))
    id))

(define (sprite-batcher:remove! sprite-batcher id)
  (batcher:remove! (sprite-batcher-batcher sprite-batcher) id)
  (sprite-batcher-batch-id+sprite-set! sprite-batcher
   (remove (lambda (x) (= (car x) id))
	   (sprite-batcher-batch-id+sprite sprite-batcher))))

(define (sprite-batcher:clear! sprite-batcher)
  (batcher:clear! (sprite-batcher-batcher sprite-batcher))
  (sprite-batcher-batch-id+sprite-set! sprite-batcher (list)))

(define (sprite-batcher:render sprite-batcher projection view)
  (batcher:render (sprite-batcher-batcher sprite-batcher) 
		  projection view))
