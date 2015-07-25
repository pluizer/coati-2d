;; This example simply opens a new window, and displays a texture

(load "../coati.so")

(define (game)

  (let* ((texture (texture:load "../share/arrow.png"))
         (batcher (sprite-batcher:create))
         (sprite  (sprite:create texture))
         (root    (node:create-root))
         (node    (spawn-node! (sprite-node batcher sprite)
                               root (trans:create (vect:create 0 0)
                                                  origin:
                                                  (vect:create .5 .5))))
         (rotation 0))

    (listen-for-event `(key-down ,key-escape)
                      (lambda (#!rest args)
                        (coati:close)))

    (lambda ()
      (with-texture/proc texture
	(lambda ()
          (sprite-batcher:render
           batcher
           (camera:create (vect:create .5 .5) 1 1 1))))

      (node:change! node (trans-change:create rotation: rotation))
      (set! rotation (+ rotation .1))
      )))

(coati:start 800 800 "Example - 01" #f game)

(exit)
