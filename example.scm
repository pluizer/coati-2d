(load "coati.so")

(use (prefix gl-math gl::))



(coati:start
 ;; Resolution
 640 480
 ;; Title
 "Coati demo"
 ;; Fullscreen?
 #f
 ;; Function returning a loop function
 (lambda ()
   (let* ((grid-texture (texture:load "share/water.png"))
          (sprite (sprite:create grid-texture))
          ;; Batcher
          (batcher (sprite-batcher:create))
          ;; Scene
          (root (node:create-root))
          (node (spawn-node! (sprite-node batcher sprite) root (trans:create (zero-vect))))
          ;; Matrices
          (projection-matrix (gl::perspective 1 1 0.1 100 20))
          (view-matrix (gl::look-at (gl::make-point 0 0 15)
                                    (gl::make-point 0 0 0)
                                    (gl::make-point 0 1 0)))
          
          )
     (listen-for-event `(key-up ,key-escape)
                       (lambda (mods)
                         (coati:close)))

     (lambda ()

       (with-texture/proc grid-texture
                          (lambda ()
                              (sprite-batcher:render batcher projection-matrix view-matrix)))

       #t)
     )))


(coati:close)

(exit)
