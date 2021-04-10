(declare (unit polygon-batcher)
         (uses batcher
               blend
               camera
               misc
               primitives
               shader))

(import srfi-1
        srfi-4
        matchable)

(define-record polygon-batcher
  batcher
  triangle-ids)

(define-record triangle-batch-id
  batch-id
  matrix
  triangle)

(define (polygon-batcher:create #!optional (shader solid-shader))
  (make-polygon-batcher
   (batcher:create shader
                   *triangle-mode*
                   3)
   (list)))

(define (polygon-batcher:push! polygon-batcher triangle/polygon matrix)
  (if (= (f32vector-length triangle/polygon) 6)
      ;; We have a triangle, push it and return the batcher-id
      (let* ((triangle-id (make-triangle-batch-id
                           (batcher:push!
                            (polygon-batcher-batcher polygon-batcher)
                            ;; Vertex data
                            (apply polygon:create
                                   (map (lambda (v) (vect*matrix v matrix))
                                        (polygon->vects triangle/polygon))))
                           matrix triangle/polygon)))
        (polygon-batcher-triangle-ids-set!
         polygon-batcher
         (cons triangle-id (polygon-batcher-triangle-ids polygon-batcher)))
        triangle-id)
      ;; We have a polygon, convert it to triangles, push-them and return
      ;; there batch-id's ...
      (map (lambda (triangle)
             (polygon-batcher:push! polygon-batcher triangle matrix))
           (polygon:triangulate->triangles triangle/polygon))))

(define (polygon-batcher:change! polygon-batcher id/s matrix)
  (if (not (list? id/s))
      ;; Change on triangle ...
      (let ((id id/s))
        (triangle-batch-id-matrix-set! id matrix)
        (match-let ((($ triangle-batch-id batch-id matrix triangle) id))
                   (batcher:change! (polygon-batcher-batcher polygon-batcher)
                                    batch-id
                                    vertex: (apply polygon:create
                                                   (map (lambda (vect)
                                                          (vect*matrix vect matrix))
                                                        (polygon->vects triangle))))))
      ;; Change multiple triangles ...
      (for-each (lambda (id)
                  (polygon-batcher:change! polygon-batcher id matrix))
                id/s)))

(define (polygon-batcher:remove! polygon-batcher id/s)
  (if (not (list? id/s))
      (let ((id id/s))
        (batcher:remove! (polygon-batcher-batcher polygon-batcher)
                         (triangle-batch-id-batch-id id))
        (polygon-batcher-triangle-ids-set! polygon-batcher
                                            (remove (=? id) (polygon-batcher-triangle-ids polygon-batcher))))
      (for-each (lambda (id)
                  (polygon-batcher:remove! polygon-batcher id))
                id/s)))

(define (polygon-batcher:clear! polygon-batcher)
  (batcher:clear! (polygon-batcher-batcher polygon-batcher))
  (polygon-batcher-triangle-ids-set! polygon-batcher (list)))

;; Render with matrices instead of camera
(define (polygon-batcher:render* polygon-batcher projection view)
  (when (not (null? (polygon-batcher-triangle-ids polygon-batcher)))
    (batcher:render (polygon-batcher-batcher polygon-batcher)
                    projection (if %target-is-screen?
                                   view
                                   ;; Flip the y-axis of all framebuffer targets.
                                   (matrix:scale (vect:create 1 -1) view))
                    (%current-colour))))

(define (polygon-batcher:render polygon-batcher)
  (polygon-batcher:render* polygon-batcher
                            (camera:projection (current-camera))
                            (camera:view (current-camera))))
