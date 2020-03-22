;; Renders a procedural generated tilemap.

(import coati)

(define (game-1)

  (let* ((texture (texture:load "../share/arrow.png"))
         (batcher (sprite-batcher:create))
         (sprite  (sprite:create texture))
         (root    (node:create-root))
         (node    (spawn-node! (sprite-node batcher sprite)
                               root (trans:create (vect:create 0 0)
                                                  origin:
                                                  (vect:create .5 .5)))))
    
   (values
     (lambda (rot)
       (with-camera (camera:create (vect:create .5 .5) 1 (vect:create 1 1))
                    (with-texture texture
                                  (sprite-batcher:render
                                   batcher
                                   )))
       
       (node:change! node (trans-change:create rotation: rot))
       (if (not (key-down? key-escape)) (+ rot .1) #f))
     1)))


(define (game-2)

  (let* ((texture-map    (texture:load "../share/grid.png"))
         (dirt-sprite    (sprite:create-from-indices texture-map 3 2 (list 0)))
         (grass-sprite   (sprite:create-from-indices texture-map 3 2 (list 4)))
         (flower-sprite  (sprite:create-from-indices texture-map 3 2 (list 5)))
         (water-sprite   (sprite:create-from-indices texture-map 3 2 (list 3)))
         ;;
         (tilemap-1      (tilemap:create))
         (tilemap-2      (tilemap:create))
         ;;
         (camera-1       (camera:create (vect:create 3.0 3.0) 1  (vect:create 6 6)))
         (camera-2       (camera:create (vect:create 2.5 2.5) 1  (vect:create 5 5)))
         ;;
         (position       (vect:create 0 0))

         ;; Sprite
         (cat-texture    (texture:load "../share/cat.png"))
         (cat-sprite     (sprite:create-from-indices cat-texture 4 13 (list 0 4 8 12)))
         (cat-size	 (texture:size cat-texture))
         ;;
         (sprite-batcher (sprite-batcher:create))
         (root-node      (node:create-root))
         (cat-node       (spawn-node! (sprite-node sprite-batcher cat-sprite) root-node
                                      (trans:create (vect:create 2 2)
                                                    origin: (vect:create (/ (vect:x cat-size) 2)
                                                                         (/ (vect:y cat-size) 2))
                                                    flip-v?: #t)))
         ;; Polygons
         (triangle-batcher (triangle-batcher:create)))


    
    (listen-for-event `(key-pressed ,key-right)
                      (lambda (#!rest _)
                        (set! position (vect+ position (vect:create -.1 0)))))

    (listen-for-event `(key-pressed ,key-escape)
                      (lambda (#!rest _)
                        (coati:close)))

    (for-each (lambda (triangle)
                (triangle-batcher:push! triangle-batcher
                                        triangle
                                        (trans->matrix (trans:create (zero-vect)))))
              (polygon:triangulate->triangles (polygon:create (vect:create 0 0)
                                                              (vect:create 0 1)
                                                              (vect:create 1 1)
                                                              (vect:create 1 .2))))

    
    (lambda (#!rest _)
      (texture:clear (rgb:create 0 0 0))
      (with-texture texture-map
                    ;; Render first layer                           
                    (with-camera camera-1
                     (tilemap:render tilemap-1
                                     6 6
                                     (lambda (coord)
                                       (if (and (zero? (coord:x coord))
                                                (zero? (coord:y coord)))
                                           water-sprite
                                           (if (and (even? (coord:x coord))
                                                    (even? (coord:y coord)))
                                               grass-sprite
                                               (if (< (coord:x coord) 50) dirt-sprite water-sprite))))))
                    ;; Render second layer
                    (with-camera camera-2
                     (with-blending trans (rgb:create 1 1 1)
                                    (tilemap:render tilemap-2
                                                    5 5
                                                    (lambda (coord)
                                                      (if (and (odd? (coord:x coord))
                                                               (odd? (coord:y coord)))
                                                          (if (< (coord:x coord) 50) flower-sprite dirt-sprite)
                                                          #f))))))

      ;; Render sprites
      (with-camera camera-2
       (with-texture cat-texture
                     (with-blending trans (rgb:create 1 1 1)
                                    (sprite-batcher:update! sprite-batcher)
                                    (sprite-batcher:render sprite-batcher)))
       (with-blending trans (rgb:create 1 0 0 .5)
                      (triangle-batcher:render triangle-batcher)))
      
      (camera-pos-set! camera-1 position)
      (camera-pos-set! camera-2 position))))

(coati:start 800 600 "Example - 02" #f game-1)
(coati:start 800 600 "Example - 02" #f game-2)

(exit)
