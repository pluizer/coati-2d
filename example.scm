(use matchable)
(use (prefix gl-math gl::))

(load "~/code/coati-2d/coati.so")

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

(define (layered-map:create/proc #!rest sizes)
  ;; Check if the size of layers increasing.
  (let ((sizes (map car sizes)))
    (assert (equal? (sort sizes <) sizes)))
  (make-layered-map
   (map (lambda (size)
	  (apply %layer:create size)) sizes)))

(define (layered-map:render-jobs lmap pos camera)
  (let ((projection (camera:projection camera))
        (view (camera:view camera)))
   (map (lambda (layer)
          (let* ((size-a (layer-size (car (layered-map-layers lmap))))
                 (size-b (layer-size layer))
                 (scale-delta (/ (+ size-a 1) (+ size-b 1))))
            (match-let ((($ layer size tilemap func) layer))
              (let ((render
                     (lambda ()
                       (tilemap:render tilemap pos size size func camera
                                       (lambda (m)
                                         (matrix:scale
                                          (vect:create scale-delta scale-delta)
                                          m))))))
                (lambda ()
                  (render))))))
        (layered-map-layers lmap))))

(define (parallax-view-matrices sizes background-view-matrix)
  (let ((background-size (car sizes)))
    (map (lambda (size)
           (let ((delta (/ (+ background-size 1)
                           (+ size))))
             (matrix:scale (vect:create delta delta)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (game)
  (print (window:size))
  (let* ((sbatch (sprite-batcher:create))
	 (lbatch (sprite-batcher:create))
	 ;; load the tilemap texture.
	 (texture (texture:load "/home/iisjmii/code/coati-2d/share/grid.png"))
	 ;; water texture
	 (water-texture (texture:load 
"/home/iisjmii/code/coati-2d/share/water.png"))
	 ;; light texture
	 (light-texture (texture:load 
"/home/iisjmii/code/coati-2d/share/light.png"))
	 ;; sprites
	 (dirt  (sprite:create-from-indices texture 3 2 (list 0)))
	 (blue  (sprite:create-from-indices texture 3 2 (list 1)))
	 (green  (sprite:create-from-indices texture 3 2 (list 4)))
	 (water (sprite:create-from-indices water-texture 5 2
					    (iota 10)))
	 (grass (sprite:create-from-indices texture 3 2 (list 4)))
	 (light (sprite:create light-texture))
	 
	 ;; matrices
         ;;(projection-matrix (perspective-matrix 1 1 1))
	 (lmap (layered-map:create/proc

                (list 4 (lambda (coord)
                          (if (even? (coord:x coord)) dirt #f)))

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

    (listen-for-event `(key-pressed ,key-down)
		      (lambda (mod)
			(set! pos (vect+ pos (vect:create 0 .1)))))
    (listen-for-event `(key-pressed ,key-up)
		      (lambda (mod)
			(set! pos (vect- pos (vect:create 0 .1)))))
    (listen-for-event `(key-pressed ,key-right)
		      (lambda (mod)
			(set! pos (vect+ pos (vect:create .1 0)))))
    (listen-for-event `(key-pressed ,key-left)
		      (lambda (mod)
			(set! pos (vect- pos (vect:create .1 0)))))

    (listen-for-event `(key-down ,key-return)
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

      (let ((camera (camera:create (vect:create 2 2) 20 1 1)))
       (match-let (((background shadow foreground)
                    (layered-map:render-jobs lmap pos camera)))
         (with-texture/proc texture
                            (lambda ()
                              (background)))
         (with-texture/proc water-texture
                            (lambda ()
                              (sprite-batcher:render sbatch camera)))
         ;; (with-texture/proc texture
         ;;                    (lambda ()
         ;;                      (shadow)
         ;;                      (foreground)))

         ;; (with-blend-mode/proc 'trans (rgb:create 1 1 1 .5)
         ;;                       (lambda ()
         ;;                        (shadow)))
         ;;(foreground)
         ))
      

;      (when (not switch) (buffer-renderer))
      
      #t)))


(coati:start 800 800 "Coati" #f game)

(exit)
