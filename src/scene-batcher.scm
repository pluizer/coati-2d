(declare (unit scene-batcher)
	 (uses node
	       scene
	       sprite-batcher))

(define-record node-data
  sprite
  batch-id)

(define-record scene-batcher
  sprite-batcher
  scene)

(define (scene-batcher:create)
  (let* ((sprite-batcher (sprite-batcher:create))
	 (scene (scene:create
		 (lambda (node)
		   (node-data-batch-id-set!
		    (node-data node)
		    (sprite-batcher:push! sprite-batcher
					  (node-data-sprite (node-data node))
					  (node:matrix node))))
		 (lambda (node)
		   (sprite-batcher:change!
		    sprite-batcher
		    (node-data-batch-id (node-data node))
		    (node:matrix node)))
		 (lambda (node)
		   (sprite-batcher:remove!
		    sprite-batcher
		    (node-data-batch-id (node-data node)))))))
    (make-scene-batcher sprite-batcher scene)))

(define (scene-batcher:push! scene-batcher sprite trans #!optional parent)
  (let ((parent (if parent parent (scene-root-node
				   (scene-batcher-scene scene-batcher)))))
    (scene:spawn! (scene-batcher-scene scene-batcher) parent trans
		  (sprite-size sprite)
		  data: (make-node-data sprite #f))))

(define (scene-batcher:remove! scene-batcher node)
  (scene:remove! (scene-batcher-scene scene-batcher) node))


(define (scene-batcher:change! scene-batcher node trans)
  (scene:change! (scene-batcher-scene scene-batcher) node trans))


(define (scene-batcher:render scene-batcher projection view)
  (sprite-batcher:render (scene-batcher-sprite-batcher scene-batcher)
			 projection view))
