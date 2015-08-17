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

(define (game-loop iter-func prev-ret)
  (poll-input-events)
  (poll-events!)
  (sdl-gl-swap-buffers)
  (let ((ret (apply iter-func (if (list? prev-ret) prev-ret
                                  (list prev-ret)))))
    (when (and ret (not %window-should-close?))
        (game-loop iter-func ret))))

;; Opens a windows a starts a ''game''.
;; Game must be a function that returns:
;; -) A function that takes any number argument -- that
;;    reperesent its internal state -- and returns a
;;    list of new values to that state.
;; Additionally it can return another value, a list of
;; values to its inital state.
;; For example ...
;; (coati:start 1024 768 "Example game" #f
;;              (lambda ()
;;                (values (lambda (position animation)
;;                          (if player-is-in-water
;;                              (list (update-position position) 'swimming)
;;                              (list (update-position position) 'walking)))
;;                        (list (vect:create 10 20) 'walking))))
(define (coati:start w h title fullscreen? game)
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
    (call-with-values
        (lambda () (game))
        (lambda (f #!optional (a (list))) (game-loop f a)))))
