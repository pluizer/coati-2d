(declare (unit window)
         (uses primitives))

(use (prefix glfw3 fw::)
     (prefix opengl-glew gl::)
     (prefix gl-utils gl::)
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
  (fw::init)
  (fw::with-window (w h 
		      title
		      fullscreen?: fullscreen?
		      resizable:  #f)
                   (set! %window-size (let-values (((w h) (fw::get-window-size (fw::window))))
                                        (vect:create w h)))
		   (gl::init)
		   (gl::enable gl::+texture-2d+)
		   (gl::enable gl::+blend+)
		   (gl::disable gl::+depth-test+)
		   (fw::swap-interval 0)
		   (gl::check-error)
		   (let ((iter (loop-func)))
		    (let loop () 
		      (iter)
		      (poll-events!)
		      (fw::swap-buffers (fw::window))
		      (fw::poll-events)
		      (if (and (iter)
			       (not %window-should-close?)
			       (not (fw::window-should-close (fw::window))))
			  (loop)
			  (begin
			    (fw::set-window-should-close (fw::window) #t)))))))


