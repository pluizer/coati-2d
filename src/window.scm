(declare (unit window))

(use (prefix glfw3 fw::)
     (prefix opengl-glew gl::)
     (prefix gl-utils gl::)
     srfi-18)

(define (window:create w h title fullscreen? iter)
  (fw::init)
  (fw::with-window (w h 
		      title
		      fullscreen?: fullscreen?
		      resizable:  #f
		      context-version-major: 3
		      context-version-minor: 3)
		   (fw::make-context-current (fw::window))
		   (gl::init)
		   (gl::enable gl::+texture-2d+)
		   (gl::enable gl::+blend+)
		   (gl::disable gl::+depth-test+)
		   (gl::check-error)
		   (let loop () 
		     (iter)
		     (fw::swap-buffers (fw::window))
		     (fw::poll-events)
		     (if (and (iter)
			      (not (fw::window-should-close (fw::window))))
			 (loop)
			 (begin
			   (free-all-resources)
			   (fw::set-window-should-close (fw::window) #t))))))

