(declare (unit texture)
	 (uses misc
               sprite
	       sprite-batcher
	       primitives))


(use srfi-1
     srfi-4
     sdl-base
     (prefix opengl-glew gl::)
     (prefix gl-utils gl::)
     (prefix soil gl::))

(define-record texture
  texture-id
  framebuffer-id
  size
  )

(define (%framebuffer-error-string status)
  (cond
   ((= status gl::+framebuffer-undefined+)
    "framebuffer undefined")
   ((= status gl::+framebuffer-incomplete-attachment+)
    "framebuffer incomplete attachment")
   ((= status gl::+framebuffer-incomplete-missing-attachment+)
    "framebuffer incomplete missing attachment")
   ((= status gl::+framebuffer-incomplete-draw-buffer+)
    "framebuffer incomplete draw buffer")
   ((= status gl::+framebuffer-incomplete-read-buffer+)
    "framebuffer incomplete read buffer")
   ((= status gl::+framebuffer-unsupported+)
    "framebuffer unsupported")
   ((= status gl::+framebuffer-incomplete-multisample+)
    "framebuffer incomplete multisample")
   (else "unknown")))

(define (%create-framebuffer texture-id)
  (let ((buffer-id (gl::gen-framebuffer)))
    (gl::with-framebuffer buffer-id
			  (gl::framebuffer-texture-2d gl::+framebuffer+
						      gl::+color-attachment0+
						      gl::+texture-2d+
						      texture-id 0))

    (let ((status (gl::check-framebuffer-status gl::+framebuffer+)))
      (unless (= status
		 gl::+framebuffer-complete+)
	(error (sprintf "could not create framebuffer: ~a."
			(%framebuffer-error-string status)))))
    (gl::check-error)
    buffer-id))

(define (%texture-linear-filter texture-id)
  (gl::with-texture gl::+texture-2d+ texture-id
	(gl::tex-parameteri gl::+texture-2d+ gl::+texture-mag-filter+ gl::+nearest+)
	(gl::tex-parameteri gl::+texture-2d+ gl::+texture-min-filter+ gl::+nearest+)))

(define (make-texture* texture-id framebuffer-id size)
  (set-finalizer! (make-texture texture-id framebuffer-id size)
                  (lambda (x)
                    (gl::delete-texture texture-id)
                    (gl::delete-framebuffer framebuffer-id))))

(define (sdl-surface->texture surface)
  (let ((id (gl::gen-texture))
        (mode (if (= (sdl-pixel-format-bytes-per-pixel
                      (sdl-surface-pixel-format surface)) 4)
                  gl::+rgba+ gl::+rgb+))
        (w (sdl-surface-width surface))
        (h (sdl-surface-height surface)))
    (gl::with-texture gl::+texture-2d+ id
                      (gl::tex-image-2d gl::+texture-2d+ 0 mode
                                        w h
                                        0 mode gl::+unsigned-byte+
                                        (sdl-surface-pixels surface)))
    (let ((texture (make-texture* id (%create-framebuffer id) (vect:create w h))))
      (%texture-linear-filter id)
      texture)))

(define (texture:load filename)
  (let ((id (gl::load-ogl-texture filename 
				  gl::force-channels/rgba
				  gl::texture-id/create-new-id 0)))
    (unless id (error (sprintf "Could not load ~a, ~a" filename (gl::last-result))))
    (let ((texture
	   (make-texture* id
                          (%create-framebuffer id)
                          (vect:create
                           (gl::ogl-texture-width id)
                           (gl::ogl-texture-height id)))))
      ;; setting linear-filter makes ogl-texture-width/height fail (return 0).
      ;; so we'll do it after calling those.
      (%texture-linear-filter id)
      texture)))

(define (texture:create size #!optional data)
  (let ((id (gl::gen-texture)))
    (gl::with-texture gl::+texture-2d+ id
	(gl::tex-image-2d gl::+texture-2d+ 0 gl::+rgba8+ 
			(vect:x size)
			(vect:y size)
			0 gl::+bgra+ gl::+unsigned-byte+ data)
	(%texture-linear-filter id)
	(gl::check-error))
    (make-texture* id (%create-framebuffer id) size)))

(define (texture:clear rgba)
  (gl::clear-color (rgb:r rgba)
		   (rgb:g rgba)
		   (rgb:b rgba)
		   (rgb:a rgba))
  (gl::clear gl::+color-buffer-bit+))

(define texture:size texture-size)

(define texture:texture-id texture-texture-id)
(define texture:framebuffer-id texture-framebuffer-id)

;; Returns a texture renderer that takes a projection- and view matrix
;; instead of a camera object.
(define (texture:renderer* texture
                           #!optional (rect (rect:create 0 1 1 0)))
  (let ((sprite (sprite:create texture (list rect)))
	(sprite-batcher (sprite-batcher:create)))
    (sprite-batcher:push! sprite-batcher sprite (identity-matrix))
    (lambda (projection view)
      (with-texture/proc texture
	(lambda ()
          (sprite-batcher:render* sprite-batcher projection view))))))

;; Returns a functions that renders a texture.
(define (texture:renderer texture
			  #!optional (rect (rect:create 0 1 1 0)))
  (let ((func (texture:renderer* texture rect)))
    (lambda (camera)
      (func (camera:projection camera)
            (camera:view camera)))))

;; Returns a function that renders a texture fullscreen.
(define (texture:fullscreen-renderer texture
				     #!optional (rect (rect:create 0 1 1 0)))
  (let ((renderer (texture:renderer* texture rect))
	(projection (identity-matrix))
	(view (f32vector 2 0 0 0
			 0 2 0 0
			 0 0 1 0
			 -1 -1 0 1)))
    (lambda ()
      (renderer projection view))))

(define (with-texture/proc texture thunk)
  (gl::with-texture gl::+texture-2d+ (texture:texture-id texture)
		    (thunk)))

(define %target-is-screen? #t)

(define (with-target/proc target thunk)
  (let ((id (texture:framebuffer-id target)))
    (set! %target-is-screen? (= id 0))
    (gl::with-framebuffer id (thunk))))

;; TODO: free texture, more testing needed
