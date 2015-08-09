(module coati
    *
  (import chicken scheme foreign)
  (declare (uses animators
                 batcher
                 blend
                 chunk
                 drawing
                 events
                 font
                 input
                 misc
                 node
                 node-pers
                 pixmap
                 particles
                 polygon-batcher
                 shader
                 sound
                 sprite
                 sprite-batcher
                 texture
                 tilemap
                 trans
                 window
                 ))

(define-syntax with-texture
  (syntax-rules ()
    ((_ texture rest ...)
     (with-texture/proc texture (lambda () rest ...)))))

(define-syntax with-target
  (syntax-rules ()
    ((_ texture rest ...)
     (with-target/proc texture (lambda () rest ...)))))

)


