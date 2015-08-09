(declare (unit polygon-batcher)
         (uses batcher
               blend
               camera
               misc
               primitives
               shader))

(use srfi-1
     srfi-4
     matchable)

(define-record triangle-batcher
  batcher
  triangle-ids)

(define-record triangle-batch-id
  batch-id
  matrix
  triangle)

(define (triangle-batcher:create #!optional (shader solid-shader))
  (make-triangle-batcher
   (batcher:create shader
                   *triangle-mode*
                   3)
   (list)))

(define (triangle-batcher:push! triangle-batcher triangle matrix)
  (let* ((triangle-id (make-triangle-batch-id
                       (batcher:push!
                        (triangle-batcher-batcher triangle-batcher)
                        ;; Vertex data
                        (apply polygon:create
                               (map (lambda (v) (vect*matrix v matrix))
                                    (polygon->vects triangle))))
                       matrix triangle)))
    (triangle-batcher-triangle-ids-set!
     triangle-batcher
     (cons triangle-id (triangle-batcher-triangle-ids triangle-batcher)))
    triangle-id))

(define (triangle-batcher:change! triangle-batcher triangle-batch-id
                                  matrix)
  (triangle-batch-id-matrix-set! triangle-batch-id matrix)
  (match-let ((($ triangle-batch-id batch-id matrix triangle)
               triangle-batcher))
    (batcher:change! (triangle-batcher-batcher triangle-batcher)
                     batch-id
                     vertex: (apply polygon:create
                                    (map (lambda (vect)
                                           (vect*matrix vect matrix)) triangle)))))

(define (triangle-batcher:remove! triangle-batcher id)
  (batcher:remove! (triangle-batcher-batcher triangle-batcher)
		   (triangle-batch-id-batch-id id))
  (triangle-batcher-triangle-ids-set! triangle-batcher
   (remove (=? id) (triangle-batcher-triangle-ids triangle-batcher))))

(define (triangle-batcher:clear! triangle-batcher)
  (batcher:clear! (triangle-batcher-batcher triangle-batcher))
  (triangle-batcher-triangle-ids-set! triangle-batcher (list)))

;; Render with matrices instead of camera
(define (triangle-batcher:render* triangle-batcher projection view)
  (when (not (null? (triangle-batcher-triangle-ids triangle-batcher)))
    (batcher:render (triangle-batcher-batcher triangle-batcher) 
                    projection (if %target-is-screen?
                                   view
                                   ;; Flip the y-axis of all framebuffer targets.
                                   (matrix:scale (vect:create 1 -1) view))
                    (%current-colour))))

(define (triangle-batcher:render triangle-batcher camera)
  (triangle-batcher:render* triangle-batcher
                          (camera:projection camera)
                          (camera:view camera)))
