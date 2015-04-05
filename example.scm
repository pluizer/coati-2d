(use matchable)
(use (prefix gl-math gl::))
(load "~/Copy/coati-2d/coati.so")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record layer
  size		;; will be the width ''and'' the height.
  tilemap
  func)

(define (%layer:create size func)
  (make-layer size (tilemap:create) func))

(define-record layered-map
  layers)

(define (layered-map:create/proc #!rest layer-args)
  (make-layered-map
   (map (lambda (args)
	  (apply %layer:create args)) layer-args)))

(define (%calculate-layer-matrix lmap layer view)
  (let* ((size-a (layer-size (car (layered-map-layers lmap))))
	 (size-b (layer-size layer))
	 (scale-delta (/ (+ size-a 1) (+ size-b 1)))
	 (trans-delta (- (/ size-a 2)
			 (/ size-b 2))))
    (if (= size-a size-b) view
	(matrix:scale (vect:create 1 1) view
         )
	)))

(define (layered-map:render lmap pos projection view)
  (for-each
   (lambda (layer)

     (let* ((size-a (layer-size (car (layered-map-layers lmap))))
            (size-b (layer-size layer))
            (scale-delta (/ (+ size-a 1) (+ size-b 1))))

       (match-let ((($ layer size tilemap func) layer))
         (tilemap:render tilemap
                         pos
                         size size
                         func
                         projection
                         view
                         (lambda (m)
                           (matrix:scale (vect:create scale-delta scale-delta)
                                         m))
                         )))
     )
   (layered-map-layers lmap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (game)
  (let* ((sbatch (sprite-batcher:create))
	 (lbatch (sprite-batcher:create))
	 ;; load the tilemap texture.
	 (texture (texture:load "/home/iisjmii/Copy/coati-2d/share/grid.png"))
	 ;; water texture
	 (water-texture (texture:load "/home/iisjmii/Copy/coati-2d/share/water.png"))
	 ;; light texture
	 (light-texture (texture:load "/home/iisjmii/Copy/coati-2d/share/light.png"))
	 ;; sprites
	 (dirt  (sprite:create-from-indices texture 3 2 (list 0)))
	 (blue  (sprite:create-from-indices texture 3 2 (list 1)))
	 (green  (sprite:create-from-indices texture 3 2 (list 4)))
	 (water (sprite:create-from-indices water-texture 5 2
					    (iota 10)))
	 (grass (sprite:create-from-indices texture 3 2 (list 4)))
	 (light (sprite:create light-texture))
	 
	 ;; matrices
         (projection-matrix (perspective-matrix 1 1 20))
         ;;(projection-matrix (gl::ortho 13 13))
	 (view-matrix (look-at-matrix (vect:create 5 5) (vect:create 5 5) 20))
	 (lmap (layered-map:create/proc

                (list 10 (lambda (coord)
                          (if (even? (coord:x coord)) dirt dirt)))

		(list 12 (lambda (coord)
			   (if (and (odd? (coord:y coord))
                                    (even? (coord:x coord))) blue #f)))

                (list 13 (lambda (coord)
			   (if (and (odd? (coord:y coord))
                                    (even? (coord:x coord))) green #f)))
                ))

	 ;; position on the map
	 (pos (zero-vect))
	 ;; nodes
	 (root (node:create-root))
	 (node-a (spawn-node! (sprite-node sbatch water)
			      root
			      (trans:create (zero-vect))))
	 (light-node
	  (spawn-node! (node-light lbatch light 4 (rgb:create 1 0 0))
		       root
		       (trans:create (zero-vect))))
	 (light-node2
	  (spawn-node! (node-light lbatch light 4 (rgb:create 1 1 0))
		       root
		       (trans:create (vect:create 1 1))))

	 (buffer (texture:create (vect:create 800 600)))
	 (buffer-renderer (texture:fullscreen-renderer buffer))

	 (switch #f)

	 
	 ;; fps counter
	 (fps (fps-counter:create print)))


    ;; Exit when escape is pressed.
    (listen-for-event `(key-down ,key-escape)
		      (lambda (mod)
			(print "Bye!")
			(coati:close)))

    (listen-for-event `(key-repeat ,key-down)
		      (lambda (mod)
			(set! pos (vect+ pos (vect:create 0 .1)))))
    (listen-for-event `(key-repeat ,key-up)
		      (lambda (mod)
			(set! pos (vect- pos (vect:create 0 .1)))))
    (listen-for-event `(key-repeat ,key-right)
		      (lambda (mod)
			(set! pos (vect+ pos (vect:create .1 0)))))
    (listen-for-event `(key-repeat ,key-left)
		      (lambda (mod)
			(set! pos (vect- pos (vect:create .1 0)))))

    (listen-for-event `(key-down ,key-enter)
		      (lambda (mod)
			(print "Switch")
			(set! switch (not switch))
			#t))


    ;; The game loop
    (lambda ()

      (fps)

      (texture:clear (rgb:create 0 0 0))

      ;; (when switch
      ;;   (with-blend-mode/proc 'one-one (rgb:create 1 1 1)
      ;;   		      (lambda ()
      ;;   			(with-texture/proc light-texture
      ;;   			  (lambda ()
      ;;   			    (sprite-batcher:render lbatch
      ;;   						   projection-matrix
      ;;   						   view-matrix))))))
      ;; (with-target/proc buffer
      ;;   (lambda ()
      ;; 	  (texture:clear (rgb:create 0 0 0))	
      ;;     (with-blend-mode/proc 'one-one (rgb:create 1 1 1)
      ;; 	    (lambda ()
      ;; 	      (with-texture/proc light-texture
      ;; 				 (lambda ()
      ;; 				   (sprite-batcher:render lbatch
      ;; 							  projection-matrix
      ;; 							  view-matrix)))))))

      (with-texture/proc texture
      	(lambda ()
      	  (layered-map:render lmap pos
      			      projection-matrix
      			      view-matrix)
          (sprite-batcher:render sbatch projection-matrix view-matrix)
))


;      (when (not switch) (buffer-renderer))
      
      #t)))


(coati:start 800 800 "Coati" #f game)

(exit)
