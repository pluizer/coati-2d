(use (prefix glfw3 fw::) 
     (prefix opengl-glew gl::)
     (prefix gl-utils gl::)
     (prefix gl-math gl::)
     (prefix soil gl::)
     srfi-1
     srfi-4
     2d-primitives
     chunk-vector
     data-structures
     srfi-69
     lolevel)

(load "coati.so")

(print gl::init)

(define projection-matrix
  (gl::perspective 640 480 0.1 100 20))

(define view-matrix
  (gl::look-at (gl::make-point .5 .5 3)
	       (gl::make-point .5 .5 0)
	       (gl::make-point 0 1 0)))


(define batcher 	(make-parameter #f))
(define texture 	(make-parameter #f))
(define buffer  	(make-parameter #f))
(define tilebatcher 	(make-parameter #f))

(define (init)
  ;; Initialise Glew
  (gl::init)
  (gl::enable gl::+texture-2d+)
  (gl::disable gl::+depth-test+)
  (gl::check-error)

  (texture (texture:load "grid.png"))
  (buffer (texture:create (vect:create 640 480)))

  (batcher (sprite-batcher:create))

  (tilebatcher (tilemap:create 1))

  (sprite-batcher:push! (batcher)
			(sprite:create (texture) 
				       (list (rect:create 0 (/ 1 3) (/ 1 2) 0)))
			(trans:create (vect:create 0 0)
				      rotation: 23
				      origin: (vect:create 1 1)))
  (sprite-batcher:push! (batcher)
			(sprite:create (texture) 
				       (list (rect:create 0 (/ 1 3) (/ 1 2) 0)))
			(trans:create (vect:create 1.5 1))
			)

)

(define x (make-parameter 0))
(define y (make-parameter 0))

(fw::key-callback (lambda (window key scancode action mods)
		(cond
		 [(and (eq? key fw::+key-escape+) (eq? action fw::+press+))
		  (fw::set-window-should-close window #t)]
		 ((and (eq? key fw::+key-right+)
		       (eq? action fw::+press+))
		  (x (+ (x) .5)))
		 ((and (eq? key fw::+key-up+)
		       (eq? action fw::+press+))
		  (y (+ (y) .5)))
		 ((and (eq? key fw::+key-left+)
		       (eq? action fw::+press+))
		  (x (- (x) .5)))
		 ((and (eq? key fw::+key-down+)
		       (eq? action fw::+press+))
		  (y (- (y) .5)))



)))


(define (fps-counter:create thunk)
  (let ((epoch (current-milliseconds))
	(count 0))
    (lambda ()
      (let ((now (current-milliseconds))
	    (ret count))
	(set! count (+ count 1))
	(when (> (- now epoch) 1000)
	  (set! epoch now)
	  (set! count 0)
	  (thunk (+ ret 1)))))))

(define (fps-warning:create min func)
  (let ((warned #f))
    (lambda (count)
      (if warned
	  (when (>= count min)
	    (set! warned #f)
	    (func (sprintf "fps: ~a fine again!" count)))
	  (when (< count min)
	    (set! warned #t)
	    (func (sprintf "fps: ~a too low!" count))))
      (func (sprintf "fps: ~a" count)))))

(define fps (fps-counter:create (fps-warning:create 60 print)))


(fw::with-window (640 480 "Example" resizable: #f)
    (init)

    (fw::swap-interval 0)
    (gl::disable gl::+depth-test+)

    (define sprite  (sprite:create (texture) (list (rect:create 0 (/ 1 3) (/ 1 2) 0)
						   (rect:create (/ 1 3)
								(* (/ 1 3) 2)
								(/ 1 2) 
								0))))
    (define sprite2 (sprite:create (texture) (list (rect:create (/ 1 3)
								(* (/ 1 3) 2)
								(/ 1 2) 
								0))))
    (let loop ((r 0))
;      (gl::clear-color 1 1 1 1)
      (gl::clear gl::+color-buffer-bit+)
      (fps)


      (with-texture/proc (texture)
	(lambda ()
	  ((tilebatcher) (vect:create (x) (y))
	   9 9
	   (lambda (c) (if (even? (coord:x c)) sprite sprite2))
	   projection-matrix
	   view-matrix)  
	  (sprite-batcher:render (batcher) projection-matrix view-matrix)))

      (fw::swap-buffers (fw::window))
      (fw::poll-events)
      (gl::check-error)


      (set! view-matrix
	    (gl::look-at (gl::make-point 0 0 20)
			 (gl::make-point 0 0 0)
			 (gl::make-point 0 1 0)))
      
      (unless (fw::window-should-close (fw::window))
	(loop (+ r .01)))))

(exit)
