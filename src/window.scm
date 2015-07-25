(declare (unit window)
         (uses primitives))

(use (prefix opengl-glew gl::)
     (prefix gl-utils gl::)
     sdl-base
     srfi-18)

(define %window-should-close? #f)
(define (coati:close)
  (set! %window-should-close? #t))
(define %window-size (vect:create 0 0))

(define (window:size) %window-size)

;; Starts coati and starts the game loop with the function
;; '''returned''' by ''loop-func''. Getting the loop function
;; in this way makes it more convinient to init objects that
;; depend on Coati to be started first.
(define (coati:start w h title fullscreen? loop-func)
  (unless (sdl-init SDL_INIT_EVERYTHING)
    (error "Could not initialise SDL." (sdl-get-error)))
  (sdl-gl-set-attribute SDL_GL_DOUBLEBUFFER 1)
  (sdl-gl-set-attribute SDL_GL_SWAP_CONTROL 1)
  (let ((surface (sdl-set-video-mode w h 32
                                     (bitwise-ior (if fullscreen? SDL_FULLSCREEN 0)
                                                  SDL_OPENGL))))
    (unless surface
      (error (sprintf "Could not set video mode (~sx~s:~s):" w h 32) (sdl-get-error)))
    (set! %window-size (vect:create w h))
    (gl::init)
    (gl::enable gl::+texture-2d+)
    (gl::enable gl::+blend+)
    (gl::disable gl::+depth-test+)
    (gl::check-error)
    (let ((iter (loop-func)))
      (let loop () 
        (poll-input-events)
        (poll-events!)
        (sdl-gl-swap-buffers)
        (when (and (iter) (not %window-should-close?))
            (loop)
            )))))
