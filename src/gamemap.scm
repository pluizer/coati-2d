(declare (unit gamemap)
         (uses tilemap
	       camera))

(import srfi-1
	matchable)

(define-record gamemap
  tilemaps
  regions %last-region
  dirty?)

(define-record region
  x y w h func)

(define (gamemap:create q-layers)
  (make-gamemap (map (lambda (_) (tilemap:create isometric?: #t))
		     (iota q-layers))
		(list) #f
		#f))

(define (gamemap:add-region! gamemap top-left size func)
  (let ((x (coord:x top-left))
	(y (coord:y top-left))
	(w (coord:x size))
	(h (coord:y size)))
    (gamemap-regions-set! gamemap
			  (cons (make-region x y w h
					     ;; render each sub-map with there own
					     ;; 0, 0 coodinate orientation.
					     (lambda (n-layer coord)
					       (func n-layer (coord- coord top-left))))
				(gamemap-regions gamemap)))))

(define (gamemap:make-dirty!)
  (gamemap-dirty?-set! #t))

(define (%inside-region? region coord)
  (let ((cx (coord:x coord))
	(cy (coord:y coord)))
    (match region
	  (($ 'region x y w h _)
	   (and (>= cx x)
		(>= cy y)
		(< cx (+ x w))
		(< cy (+ y h)))))))

(define (%get-region-func gamemap coord)
  (let* ((regions (gamemap-regions gamemap))
	 (last-region (gamemap-%last-region gamemap))
	 (result (find-first (lambda (region)
			       (if region (%inside-region? region coord) #f))
			     ;; try the last region found first as a
			     ;; optimalization
			     (cons last-region regions))))
    (gamemap-%last-region-set! gamemap result)
    (if result (region-func result) #f)))

(define (gamemap:renderer gamemap)
  (let ((changed? (make-change-check))
	(func #f))
    (match gamemap
	   (($ 'gamemap tilemaps regions _ dirty?)
	    (lambda ()
	      (map (lambda (tilemap n-layer)
		     
		     (let ((func #f))
		       (tilemap:render tilemap #f dirty?
				       (lambda ()
					 (lambda (coord)
					     (display coord) (display ", ") (newline) 
					   (when (changed? coord)
					     (set! func (%get-region-func gamemap coord)))
					   (if func (func n-layer coord) #f))))))
		   tilemaps
		   (iota (length tilemaps))))))))
