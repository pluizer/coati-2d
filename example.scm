(use (prefix glfw3 fw::) 
     (prefix opengl-glew gl::)
     (prefix gl-utils gl::)
     (prefix gl-math gl::)
     (prefix soil gl::)
     srfi-1
     srfi-4
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
(define bid	 	(make-parameter #f))
(define ida	 	(make-parameter #f))
(define idb	 	(make-parameter #f))
(define idc	 	(make-parameter #f))
(define s	 	(make-parameter #f))

(define (sprite-name sprite)
  (cond ((equal? (ida) sprite) 'ida)
	((equal? (idb) sprite) 'idb)
	((equal? (idc) sprite) 'idc)
	(else 'root)))

(define (init)
  ;; Initialise Glew
  (gl::init)
  (gl::enable gl::+texture-2d+)
  (gl::disable gl::+depth-test+)
  (gl::check-error)

  (texture (texture:load "grid.png"))
  (buffer (texture:create (vect:create 640 480)))

  (batcher (sprite-batcher:create))

  (tilebatcher (tilemap:create))

  (s (scene-batcher:create))

  (let ((dirt  (sprite:create-from-indices (texture) 3 2 (list 0)))
	(water (sprite:create-from-indices (texture) 3 2 (list 1)))
	(grass (sprite:create-from-indices (texture) 3 2 (list 4))))


    (ida (scene-batcher:push! (s) dirt  (trans:create
					 (vect:create 0 0)
					 rotation: .1)))

    (idb (scene-batcher:push! (s) water (trans:create
					 (vect:create 1 1)
					 rotation: .1)
			      (ida)))
    (idc (scene-batcher:push! (s) grass (trans:create
					 (vect:create 1 1))
			      (idb)))

    )


  
					;    (print (frame-rectangle (%frame:create (texture) (rect:create 0 (/ 1 3) (/ 1 2) 0))))
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


(define fps (fps-counter:create (fps-warning:create 60 print print)))


(fw::with-window (1440 1050 "Example" resizable: #f)
    (init)

    (fw::swap-interval 0)
    (gl::disable gl::+depth-test+)

    (define sprite  (sprite:create (texture) (list (rect:create 0 (/ 1 3) 0 (/ 1 2))
						   (rect:create (/ 1 3)
								(* (/ 1 3) 2)
								(/ 1 2)
								0 ))
				   100))
    (define sprite2 (sprite:create-from-indices (texture)
				    3 2
				    (list 0)))

    (define sprite3 (sprite:create-from-indices (texture)
				    3 2
				    (list 4)))
    (let loop ((r 0))
;      (gl::clear-color 1 1 1 1)
      (gl::clear gl::+color-buffer-bit+)
      (fps)


      (with-texture/proc (texture)
	(lambda ()
	  ;; (tilemap:render (tilebatcher) (vect:create (x) (y))
	  ;;  12 12
	  ;;  (lambda (c) 
	  ;;    (if (and (= (coord:x c) 0)
	  ;; 	      (= (coord:y c) 0))
	  ;; 	 sprite
	  ;; 	 (if (even? (coord:x c)) sprite2 sprite3)))
	  ;;  projection-matrix
	  ;;  view-matrix)

 
;	  (sprite-batcher:render (batcher) projection-matrix view-matrix)

	  (scene-batcher:render (s) projection-matrix view-matrix)
	  

	  (scene-batcher:change! (s) (ida) (trans:create
					    (vect:create 1 1)
					    rotation: r))

	  (scene-batcher:change! (s) (idb) (trans:create
					    (vect:create 1 1)
					    rotation: r))
	  
	  (scene-batcher:change! (s) (idc) (trans:create
					    (vect:create 1 1)
					    rotation: r))

	  
	  ))
      (fw::swap-buffers (fw::window))
      (fw::poll-events)
      (gl::check-error)

      (sprite-batcher:update! (batcher))

      (set! view-matrix
	    (gl::look-at (gl::make-point 0 0 20)
			 (gl::make-point 0 0 0)
			 (gl::make-point 0 1 0)))
      
      (unless (fw::window-should-close (fw::window))
	(loop (+ r .001)))))

(exit)
