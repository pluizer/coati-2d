;; Renders a procedural generated tilemap.

(load "../coati.so")

(define (game)

  (let* ((texture-map  (texture:load "../share/grid.png"))
         (dirt-sprite  (sprite:create-from-indices texture-map 3 2 (list 0)))
         (grass-sprite (sprite:create-from-indices texture-map 3 2 (list 4)))
         (water-sprite (sprite:create-from-indices texture-map 3 2 (list 4)))
         ;;
         (tilemap      (tilemap:create))
         ;;
         (camera       (camera:create (vect:create 2.5 2.5) 1 5 5))
         ;;
         (position     (vect:create 0 0)))

    (listen-for-event `(key-pressed ,key-right)
                      (lambda (#!rest _)
                        (set! position (vect+ position (vect:create -.1 0)))))
    
    

    (lambda ()
      (with-texture/proc texture-map
	(lambda ()
          (tilemap:render tilemap
                          position
                          5 5
                          (lambda (coord)
                            (if (and (even? (coord:x coord))
                                     (even? (coord:y coord)))
                                grass-sprite
                                dirt-sprite))
                          camera))))))

(coati:start 800 600 "Example - 01" #f game)

(exit)
