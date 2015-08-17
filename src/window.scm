(declare (unit window)
         (uses primitives))

(use (prefix opengl-glew gl::)
     (prefix gl-utils gl::)
     sdl2
     sdl2-raw
     sdl2-types
     srfi-18)

(define %window-should-close? #f)
(define (coati:close)
  (set! %window-should-close? #t))
(define %window-size (vect:create 0 0))

(define (window:size) %window-size)

(define %current-window 0)

(define (game-loop iter-func prev-ret)
  (poll-input-events)
  (poll-events!)
  (SDL_GL_SwapWindow %current-window)
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
  (unless (sdl-init)
    (error "Could not initialise SDL." (sdl-get-error)))
  ;; (sdl-gl-set-attribute SDL_GL_DOUBLEBUFFER 1)
  ;; (sdl-gl-set-attribute SDL_GL_SWAP_CONTROL 1)
  (SDL_GL_SetAttribute SDL_GL_CONTEXT_MAJOR_VERSION 2)
  (SDL_GL_SetAttribute SDL_GL_CONTEXT_MINOR_VERSION 1)
  (let ((window (sdl-create-window title 0 0 w h #:opengl #:shown #:input-grabbed
                                   )))
    (unless window
      (error (sprintf "Could not set video mode (~sx~s:~s):" w h 32) (sdl-get-error)))
    (let ((context (SDL_GL_CreateContext window)))
      (unless context
        (error "No GL context"))
      (SDL_GL_MakeCurrent window context)
      (set! %current-window window)))
  
  (set! %window-size (vect:create w h))
  (gl::init)
  (gl::enable gl::+texture-2d+)
  (gl::enable gl::+blend+)
  (gl::disable gl::+depth-test+)
  (gl::check-error)
  (call-with-values
      (lambda () (game))
    (lambda (f #!optional (a (list))) (game-loop f a))))
