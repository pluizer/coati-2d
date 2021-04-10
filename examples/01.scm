;; Renders a rotating arrow.

(import coati)

(define (game)
  (let* ((texture (texture:load "../share/arrow.png"))
         (batcher (sprite-batcher:create))
         (sprite  (sprite:create texture))
         (root    (node:create-root))
         (node    (spawn-node! (sprite-node batcher sprite)
                               root (trans:create (vect:create 0 0)
                                                  origin:
                                                  (vect:create .5 .5)))))
    (listen-for-event `(key-down ,key-escape)
                      (lambda (#!rest args)
                        (coati:close)))
    (values
     (lambda (rot)
       (with-camera (camera:create (vect:create .5 .5) (vect:create 1 1))
                    (with-texture texture
                                  (sprite-batcher:render
                                   batcher)))
       (node:change! node (trans-change:create rotation: rot))
       (+ rot .1))
     1)))

(coati:init 800 600 "Example - 01" #f)
(coati:start game)

(exit)
