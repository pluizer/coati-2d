(module coati
    *
  (import scheme
          (chicken base)
          (chicken foreign))
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
		 sprite-map
                 sprite-batcher
                 texture
                 tilemap
                 trans
                 window))

(define-syntax with-texture
  (syntax-rules ()
    ((_ texture body ...)
     (with-texture/proc texture (lambda () body ...)))))

(define-syntax with-target
  (syntax-rules ()
    ((_ texture body ...)
     (with-target/proc texture (lambda () body ...)))))

(define-syntax with-blending
  (syntax-rules ()
    ((_ type colour body ...)
     (with-blending/proc 'type colour (lambda () body ...)))))

(define-syntax with-camera
  (syntax-rules ()
    ((_ camera body ...)
     (with-camera/proc camera (lambda () body ...)))))

)
