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
  (sdl-init SDL_INIT_EVERYTHING) ;; TODO: Check error
  (let ((surface (sdl-set-video-mode w h 32
                                     (bitwise-ior SDL_DOUBLEBUF
                                                  (if fullscreen? SDL_FULLSCREEN 0)
                                                  SDL_OPENGL))))
    (gl::init)
    (gl::enable gl::+texture-2d+)
    (gl::enable gl::+blend+)
    (gl::disable gl::+depth-test+)
    (gl::check-error)
    (let ((iter (loop-func)))
      (let loop () 
        (iter)
        (poll-input-events)
        (poll-events!)
        (sdl-gl-swap-buffers)
        ;; (fw::poll-events)
        ;; (if (and (iter)
        ;;          (not %window-should-close?)
        ;;          (not (fw::window-should-close (fw::window))))
        ;;     (loop)
        ;;     (begin
        ;;       (fw::set-window-should-close (fw::window) #t)))
        (sdl-delay 1) ;; TODO remove
        (loop)
        ))
    ))
