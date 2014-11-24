(declare (unit window))

(use (prefix glfw3 fw::)
     (prefix opengl-glew gl::)
     (prefix gl-utils gl::)
     srfi-18)

(define %window-should-close? #f)
(define (coati:close)
  (set! %window-should-close? #t))

;; Starts coati and starts the game loop with the function
;; '''returned''' by ''loop-func''. Getting the loop function
;; in this way makes it more convinient to init objects that
;; depend on Coati to be started first.
(define (coati:start w h title fullscreen? loop-func)
  (fw::init)
  (fw::with-window (w h 
		      title
		      fullscreen?: fullscreen?
		      resizable:  #f
)
;		   (fw::make-context-current (fw::window))
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
			    (free-all-resources)
			    (fw::set-window-should-close (fw::window) #t)))))))

