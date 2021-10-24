(declare (unit algorithms)
	 (uses primitives))

;; Returns a list of coordinates that a line between
;; start and end would intersect.
(define (raycast start end)
  (let* ((x0 (coord:x start))
	 (y0 (coord:y start))
	 (x1 (coord:x end))
	 (y1 (coord:y end))
	 (dx (abs (- x1 x0)))
	 (dy (abs (- y1 y0)))
	 (n (+ 1 dx dy))
	 (x-inc (if (> x1 x0) 1 -1))
	 (y-inc (if (> y1 y0) 1 -1))
	 (error (- dx dy))
	 (dx (* dx 2))
	 (dy (* dy 2)))
    (letrec ((calc
	      (lambda (res n x y error)
		(if (> n 1)
		    (apply calc (cons
				 (cons (coord:create x y) res)
				 (cond
				  ((> error 0)
				   (list (- n 1) (+ x x-inc) y (- error dy)))
				  ((< error 0)
				   (list (- n 1) x (+ y y-inc) (+ error dx)))
				  ((= error 0)
				   (list (- n 2) (+ x x-inc) (+ y y-inc) (+ error (- dy) dx))))))
		    res))))
      (cons end (calc (list) n x0 y0 error)))))
