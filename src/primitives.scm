(declare (unit primitives))

(use gl-math)

#>
#include <float.h>
<#

;;-------------------------------------------------------
;; Float
;;-------------------------------------------------------

;; Returns the floating-point remainder of numer/denom (rounded towards zero).
(define fmod (foreign-lambda float "fmod" float float))

;; Clamp a floating-point number to a minimum and a maximum value.
(define (clamp f mmin mmax)
  (min (max f mmin) mmax))

;; Square a number.
(define (sqr x) (* x x))

;; Minimum representable floating-point number.
(define float-min (foreign-value "DBL_MIN" float))

(define infinity (foreign-value "INFINITY" float))

;;-------------------------------------------------------
;; Constants
;;-------------------------------------------------------

(define epsilon 1e-6)

(define pi 3.14159265)

(define pi/2 1.57079633)

(define 2pi 6.283185307)

(define pi (- 3.14159265))

(define 360/2pi 57.29577951)

(define 2pi/360 0.017453293)

;;-------------------------------------------------------
;; Angle
;;-------------------------------------------------------

(define (radian->degree v)
  (* v 57.2957795))

(define (degree->radian v)
  (* v 0.0174532925))

(define (flip-radian-h angle)
  (let ((v (angle->vect angle)))
    (vect->angle (vect:create (vect:x v) (- (vect:y v))))))

(define (flip-radian-v angle)
  (let ((v (angle->vect angle)))
    (vect->angle (vect:create (- (vect:x v)) (vect:y v)))))

(define (flip-degree-h angle)
  (radian->degree
   (flip-radian-h (degree->radian angle))))

(define (flip-degree-v angle)
  (radian->degree
   (flip-radian-v (degree->radian angle))))

;;-------------------------------------------------------
;; Vectors
;;-------------------------------------------------------

;; Returs a new vector
(define (vect:create x y)
  (f32vector x y))

(define (vect? obj)
  (and (f32vector? obj)
       (= (f32vector-length obj) 2)))

(define (vect:x v)
  (f32vector-ref v 0))

(define (vect:y v)
  (f32vector-ref v 1))

;; Constant for the zero vector.
(define (zero-vect)
  (vect:create 0 0))

;; Check if two vectors are equal.
(define (vect=? a b #!optional (epsilon .001))
  (and (< (abs (- (vect:x a) (vect:x b))) epsilon)
       (< (abs (- (vect:x a) (vect:y b))) epsilon)))

;; Add two vectors.
(define (vect+ a b)
  (vect:create (+ (vect:x a) (vect:x b))
	       (+ (vect:y a) (vect:y b))))

;; Subtract two vectors or negate a vector.
(define (vect- a #!optional b)
  (if b (vect:create (- (vect:x a) (vect:x b))
		     (- (vect:y a) (vect:y b)))
      (vect:create (- (vect:x a))
		   (- (vect:y a)))))


;; Scalar multiplication.
(define (vect* v s)
  (vect:create (* (vect:x v) s)
	       (* (vect:y v) s)))

;; Vector dot product.
(define (vect:dot a b)
  (+ (* (vect:x a)
	(vect:x b))
     (* (vect:y a)
	(vect:y b))))

;; 2D vector cross product analog.
;; The cross product of 2D vectors results in a 3D vector with only a z component.
;; This function returns the magnitude of the z value.
(define (vect:cross a b)
  (- (* (vect:x a)
	(vect:y b))
     (* (vect:y a)
	(vect:x b))))
;; Returns a perpendicular vector. (90 degree rotation)
(define (vect:perp v)
  (vect:create (- (vect:y v)) (vect:x v)))

;; Returns a perpendicular vector. (-90 degree rotation)
(define (vect:vperp v)
  (vect:create (vect:y v) (- (vect:x v))))

;; Returns the vector projection of /a/ onto /b/.
(define (vect:project a b)
  (vect* a (/ (vect:dot a b)
	      (vect:dot b b))))

;; Returns the unit length vector for the given angle (in radians).
(define (angle->vect a)
  (vect:create (cos a) (sin a)))

;; Returns the angular direction v is pointing in (in radians).
(define (vect->angle v)
  (atan (vect:y v) (vect:x v)))

;; Uses complex number multiplication to rotate /a/ by /b/. Scaling will occur if /a/ is not a unit vector.
(define (vect:rotate a b)
  (vect:create (+ (* (vect:x a) (vect:x b))
		  (* (vect:y a) (vect:y b)))
	       (- (* (vect:x a) (vect:y b)
		     (vect:y a) (vect:x b)))))

;; Inverse of vect:rotate
(define (vect:unrotate a b)
  (vect:create (+ (* (vect:x a) (vect:x b))
		  (* (vect:y a) (vect:y b)))
	       (- (* (vect:y a) (vect:x b)
		     (vect:x a) (vect:y b)))))

;; Returns the squared length of v. Faster than cpvlength() when you only need to compare lengths.
(define (vect:length-squared v)
  (vect:dot v v))

;; Returns the length of v.
(define (vect:length v)
  (sqrt (vect:dot v v)))

;; Linearly interpolate between /a/ and /b/.
(define (vect:lerp v1 v2 t)
  (vect+ (vect* v1 (- 1.0 t))
	 (vect* v2 t)))

;; Returns a normalized copy of v.
(define (vect:normalize v)
  (vect* v (/ 1.0 (+ (vect:length v) float-min))))

;; Flip the vector horizontally.
(define (vect:flip-v v)
  (vect:create (- (vect:x v))
	       (vect:y v)))

;; Flip the vector vertically..
(define (vect:flip-h v)
  (vect:create (vect:x v)
	       (- (vect:y v))))

;; Flip a vector.
(define (vect:flip vertical? horzontal? vect)
  (let* ((vect (if vertical?  (vect:flip-v vect) vect))
	 (vect (if horzontal? (vect:flip-h vect) vect)))
    vect))

;; Clamp v to length len.
(define (vect:clamp v len)
  (if (> (vect:dot v v) (sqr len))
      (vect* (vect:normalize v) len)
      v))

;; Linearly interpolate between v1 towards v2 by distance d.
(define (vect:lerp-const v1 v2 dist)
  (vect+ v1 (vect+ (vect:clamp v2 v1) dist)))

;; Returns the distance between v1 and v2.
(define (vect:dist v1 v2)
  (vect:length (vect- v1 v2)))

;; Returns the squared distance between v1 and v2. Faster than cpvdist() when you only need to compare distances.
(define (vect:dist-squared v1 v2)
  (vect:length-squared (vect- v1 v2)))

;; Returns true if the distance between v1 and v2 is less than dist.
(define (vect:near? a b dist)
  (< (vect:dist-squared a b) (sqr dist)))

;; Spherical linearly interpolate between /a/ and /b/.
(define (vect:spherical-lerp a b t)
  (let* ((dot (vect:dot (vect:normalize a) (vect:normalize b)))
	 (omega (clamp dot -1.0 1.0)))
    (if (< omega 0.001)
	(vect:lerp a b t)
	(let ((denom (/ 1.0 (sin omega))))
	  (vect+ (vect* a (* (sin (* (- 1.0 t) omega)) denom))
		 (vect* b (* (sin (* (* t omega) denom)))))))))

;; Spherical linearly interpolate between /a/ towards /b/ by no more than angle /angle/ in radians.
(define (vect:spherical-lerp-const a b angle)
  (let* ((dot (vect:dot (vect:normalize a) (vect:normalize b)))
	 (omega (clamp dot -1.0 1.0)))
    (vect:spherical-lerp a b (/ (min angle omega) omega))))

(define (vect*matrix vect matrix)
  (let ((pos (m*vector! matrix (f32vector (vect:x vect) (vect:y vect) 0))))
    (vect:create (f32vector-ref pos 0)
		 (f32vector-ref pos 1))))

;;-------------------------------------------------------
;; Coords
;;-------------------------------------------------------

(define (coord:create x y)
  (s32vector x y))

(define (coord:x c)
  (s32vector-ref c 0))

(define (coord:y c)
  (s32vector-ref c 1))

;; Constant for the zero coord.
(define (zero-coord)
  (coord:create 0 0))

;; Check if two coords are equal.
(define (coord=? a b #!optional (epsilon .001))
  (and (< (abs (- (coord:x a) (coord:x b))) epsilon)
       (< (abs (- (coord:x a) (coord:y b))) epsilon)))

;; Add two coords.
(define (coord+ a b)
  (coord:create (+ (coord:x a) (coord:x b))
		(+ (coord:y a) (coord:y b))))

;; Subtract two coords or negate a coord.
(define (coord- a #!optional b)
  (if b (coord:create (- (coord:x a) (coord:x b))
		      (- (coord:y a) (coord:y b)))
      (coord:create (- (coord:x a))
		    (- (coord:y a)))))


;;-------------------------------------------------------
;; Bounding Boxes
;;-------------------------------------------------------

;; Returs a new bounding box.
(define (rect:create l r b t)
  (f32vector l r b t))

;; Can also be a line.
(define (rect? obj)
  (and (f32vector? obj)
       (= (f32vector-length obj) 4)))

(define (rect:l rect)
  (f32vector-ref rect 0))

(define (rect:r rect)
  (f32vector-ref rect 1))

(define (rect:b rect)
  (f32vector-ref rect 2))

(define (rect:t rect)
  (f32vector-ref rect 3))

;; Constructs a /rect/ for a circle with the given position and radius.
(define (rect:for-circle p r)
  (rect:create (- (vect:x p) r)
	       (+ (vect:x p) r)
	       (- (vect:y p) r)
	       (+ (vect:y p) r)))

;; Returns true if /a/ and /b/ intersect.
(define (rect:intersects? a b)
  (and (<= (rect:l a) (rect:r b))
       (<= (rect:l b) (rect:r a))
       (<= (rect:b a) (rect:t b))
       (<= (rect:b b) (rect:t a))))

;; Returns true if /other/ lies completely within /rect/.
(define (rect:contains? rect other)
  (and (<= (rect:l rect) (rect:l other))
       (>= (rect:r rect) (rect:r other))
       (<= (rect:b rect) (rect:b other))
       (>= (rect:t rect) (rect:t other))))

;; Returns true if /rect/ contains /v/.
(define (rect:constains-vect? rect v)
  (and (<= (rect:l rect) (vect:x v))
       (>= (rect:r rect) (vect:x v))
       (<= (rect:b rect) (vect:y v))
       (>= (rect:t rect) (vect:y v))))

;; Returns a bounding box that holds both bounding boxes.
(define (rect:merge a b)
  (rect:create (min (rect:l a) (rect:l b))
	       (max (rect:r a) (rect:r b))
	       (min (rect:b a) (rect:b b))
	       (max (rect:t a) (rect:t b))))

;; Returns a bounding box that holds all /vects/.
(define (rect:container vects)
  (let* ((s-x (sort (map vect:x vects) <))
	 (s-y (sort (map vect:y vects) <))
	 (min-x (car s-x))
	 (min-y (car s-y))
	 (max-x (last s-x))
	 (max-y (last s-y)))
    (rect:create min-x
		 max-x
		 min-y
		 max-y)))

;; Returns a bounding box that holds both /rect/ and /vects/.
(define (rect:expand rect vects)
  (let* ((s-x (sort (map vect:x vects) <))
	 (s-y (sort (map vect:y vects) <))
	 (min-x (car s-x))
	 (min-y (car s-y))
	 (max-x (last s-x))
	 (max-y (last s-y)))
   (rect:create (min (rect:l rect) min-x)
		(max (rect:r rect) max-x)
		(min (rect:b rect) min-y)
		(max (rect:t rect) max-y))))

;; Returns the center of a bounding box.
(define (rect:center rect)
  (vect:lerp (vect:create (rect:l rect) (rect:b rect))
	     (vect:create (rect:r rect) (rect:t rect))
	     0.5))

;; Returns the area of the bounding box.
(define (rect:area rect)
  (* (- (rect:r rect) (rect:l rect))
     (- (rect:t rect) (rect:b rect))))

;; Merges /a/ and /b/ and returns the area of the merged bounding box.
(define (rect:merged-area a b)
  (* (- (max (rect:r a) (rect:r b))
	(min (rect:l a) (rect:l b)))
     (- (max (rect:t a) (rect:t b))
	(min (rect:b a) (rect:b b)))))

;; Returns the fraction along the segment query the bounding box is hit. Returns /infinity/ if it doesn't hit.
(define (rect:segment-query rect a b)

  (let* ((idx (/ 1 (- (vect:x b) (vect:x a))))
	 (tx1 (if (= (rect:l rect) (vect:x a))
		  (- infinity)
		  (* (- (rect:l rect) (vect:x a)) idx)))
	 (tx2 (if (= (rect:r rect) (vect:x a))
		  (- infinity)
		  (* (- (rect:r rect) (vect:x a)) idx)))
	 (txmin (min tx1 tx2))
	 (txmax (max tx1 tx2))
	 ;;
	 (idy (/ 1 (- (vect:y b) (vect:y a))))
	 (ty1 (if (= (rect:b rect) (vect:y a))
		  (- infinity)
		  (* (- (rect:b rect) (vect:y a)) idy)))
	 (ty2 (if (= (rect:t rect) (vect:y a))
		  (- infinity)
		  (* (- (rect:t rect) (vect:y a)) idy)))
	 (tymin (min ty1 ty2))
	 (tymax (max ty1 ty2)))
    (if (and (<= tymin txmax)
	     (<= txmin tymax))
	(let ((mmin (max txmin tymin))
	      (mmax (min txmax tymax)))
	  (if (and (<= 0.0 mmax)
		   (<= mmin 1.0))
	      (max mmin 0.0)
	      infinity))
	infinity)))

;; Return true if the bounding box intersects the line segment with ends /a/ and /b/.
(define (rect:intersects-segment? rect a b)
  (not (= (rect:segment-query rect a b) infinity)))

(define (rect:clamp-vect rect v)
  (vect:create (clamp (vect:x v) (rect:l rect) (rect:r rect))
	       (clamp (vect:y v) (rect:b rect) (rect:t rect))))

;; Substracts a vector from a rectangle.
(define (rect- rect vect)
  (rect:create (- (rect:l rect) (vect:x vect))
	       (- (rect:r rect) (vect:x vect))
	       (- (rect:b rect) (vect:y vect))
	       (- (rect:t rect) (vect:y vect))))

;; Adds a vector to a rectangle.
(define (rect+ rect vect)
  (rect:create (+ (rect:l rect) (vect:x vect))
	       (+ (rect:r rect) (vect:x vect))
	       (+ (rect:b rect) (vect:y vect))
	       (+ (rect:t rect) (vect:y vect))))

(define (rect:size rect)
  (vect:create (abs (- (rect:l rect)
		       (rect:r rect)))
	       (abs (- (rect:b rect)
		       (rect:t rect)))))

(define (rect->polygon rect)
  (let ((l (rect:l rect))
	(r (rect:r rect))
	(t (rect:t rect))
	(b (rect:b rect)))
    (f32vector l t l b r b r t)))

;;-------------------------------------------------------
;; Lines
;;-------------------------------------------------------

;; Makes a line from two vectors
(define (line:create a b)
  (f32vector (vect:x a) (vect:y a)
	     (vect:x b) (vect:y b)))

;; Can also be a rect.
(define (line? obj)
  (and (f32vector? obj)
       (= (f32vector-length obj) 4)))

;;-------------------------------------------------------
;; Polygon
;;-------------------------------------------------------

;; Creates a new polygon from a list of vectors.
(define (polygon:create #!rest vects)
  (list->f32vector (append-map f32vector->list vects)))

;; Converts a polygon to a list of vertices.
(define (polygon->vects polygon)
  (%f32vector-part polygon 2))

;; ;; Triangulates the given polygon and returns an array of vectors.
;; (define (polygon:triangulate polygon)
;;   (let* ((return-size (- (* (f32vector-length polygon) 3) 12))
;; 	 (res (make-f32vector return-size)))
;;     ((foreign-lambda* void ((f32vector polygon)
;; 			    (f32vector res)
;; 			    (integer polygonSize)
;; 			    (integer returnSize)) "
;; 	std::vector<Vector> vec;
;; 	vec.resize(polygonSize/2);
;; 	memcpy(vec.data(), polygon, sizeof(Vector)*(polygonSize/2));
;; 	std::vector<Triangle> tmp = triangulate(vec);
;; 	memcpy(res, tmp.data(), returnSize*sizeof(Vector));")
;;      polygon res (f32vector-length polygon) return-size)
;;     res))

;; ;; Same as polygon-triangulate but returns a list of triangles.
;; (define (polygon:triangulate->triangles polygon)
;;   (%f32vector-part (polygon:triangulate polygon) 6))

;; ;; Return #t if the given polygon is convex.
;; (define (polygon-convex? polygon)
;;   ((foreign-lambda* bool ((f32vector polygon)
;; 			  (unsigned-integer length)) "
;; 	C_return( isConvex((Vector*)polygon, length) );")
;;    polygon (f32vector-length polygon)))

;; (define (%sort-vects vects)
;;   (sort vects
;; 	(lambda (a b) 
;; 	  (or (< (vect:x a) (vect:x b))
;; 	      (= (vect:x a) (vect:y b))))))

;; (define (%cross o a b)
;;   (- (* (- (vect:x a)
;; 	   (vect:x o))
;; 	(- (vect:y b)
;; 	   (vect:y o)))
;;      (* (- (vect:y a)
;; 	   (vect:y o))
;; 	(- (vect:x b)
;; 	   (vect:x o)))))

;; Returns the convex hull of a group of vertices in clockwise order.
(define (convex-hull vects)
  (let* ((sorted (%sort-vects vects))
	 (lower (list))
	 (upper (list)))
    (if (<= (length vects) 1) vects
	(begin
	  (map (lambda (v)
		 (let loop ()
		   (when (and (>= (length lower) 2)
			      (<= (%cross (cadr lower) (car lower) v) 0))
		     (set! lower (cdr lower))
		     (loop)))
		 (set! lower (cons v lower)))
	       vects)
	  (map (lambda (v)
		 (let loop ()
		   (when (and (>= (length upper) 2)
			      (<= (%cross (cadr upper) (car upper) v) 0))
		     (set! upper (cdr upper))
		     (loop)))
		 (set! upper (cons v upper)))
	       (reverse vects))
	  (reverse (append (cdr lower) (cdr upper)))))))

;; Converts any polygon to a convex polygon.
(define (polygon:convex-hull polygon)
  (convex-hull (polygon->vects polygon)))

(define (polygon+ polygon vect)
  (apply polygon:create (map (lambda (v) (vect+ v vect))
			     (polygon->vects polygon))))

(define (polygon- polygon vect)
  (apply polygon:create (map (lambda (v) (vect- v vect))
			     (polygon->vects polygon))))

(define (polygon:rotate polygon angle #!optional (origin (zero-vect)))
  (let ((ca (cos angle))
	(sa (sin angle))
	(ox (vect:x origin))
	(oy (vect:y origin)))
   (apply polygon:create
	  (map (lambda (v)
		 (let ((x (vect:x v))
		       (y (vect:y v)))
		  (vect:create
		   (- (* (- x ox) ca)
		      (* (- y oy) sa))
		   (+ (* (- x ox) sa)
		      (* (- y oy) ca))))) 
	       (polygon->vects polygon)))))

;; Returns all the axes of a list of vertices.
(define (%axes vects)
  (map (lambda (a b)
	 (vect:perp (vect- a b)))
       vects (append (cdr vects) (list (car vects)))))

;; Returns the two non-perpendicular axis of a rectangle
(define (%axes-rect vects)
  (list (vect:perp (vect- (car    vects)
			  (cadr   vects)))
	(vect:perp (vect- (caddr  vects)
			  (cadddr vects)))))

;; Projects a list of vertices onto an axis.
(define (%project vects axis)
  (let ((dots (sort (map (lambda (vect)
			   (vect:dot axis vect))
			 vects)
		    <)))
    (vect:create (car dots) (last dots))))

;; Checks if two projections overlap.
(define (%overlap? p1 p2)
  (not (or (< (vect:y p1)
	      (vect:x p2))
	   (< (vect:y p2)
	      (vect:x p1)))))

;; Checks if two list of vertices, describing a convex polygon,
;; collide with each other.
;; If the polygon is a rectangle, pass #t to rect for a more
;; optimised algorithm.
(define (vects:collide? a b #!optional rect?)
  (every (lambda (vects)
	   (every (lambda (axis)
		    (%overlap? (%project a axis)
			       (%project b axis)))
		  (if rect?
		      (%axes-rect vects)
		      (%axes vects))))
	 (list a b)))

;; Returns #t if both convex polygons collide with each other.
;; If the polygon is a rectangle, pass #t to rect for a more
;; optimised algorithm.
(define (polygon:collide? a b #!optional rect?)
  (vects:collide? (polygon->vects a)
		  (polygon->vects b)
		  rect?))

;;-------------------------------------------------------
;; Colour
;;-------------------------------------------------------

;; Creates a new RGB colour
(define (rgb:create r g b #!optional (a 1.0))
  (f32vector r g b a))

(define (rgb:r rgb)
  (f32vector-ref rgb 0))

(define (rgb:g rgb)
  (f32vector-ref rgb 1))

(define (rgb:b rgb)
  (f32vector-ref rgb 2))

(define (rgb:a rgb)
  (f32vector-ref rgb 3))

(define (rgb->hsv rgb)
  (let* ((r (rgb:r rgb))
	 (g (rgb:g rgb))
	 (b (rgb:b rgb))
	 (a (rgb:a rgb))
	 (mmin (min r g b))
	 (mmax (max r g b))
	 (c (- mmax mmin)))
    (if (not (= c 0.0))
	(let* ((v mmax)
	       (s (/ c v)))
	  (cond ((= mmax r)
		 (hsv:create (%wrap-degree
			      (* (fmod (/ (- g b) c) 6.0) 60.0)) s v a))
		((= mmax g)
		 (hsv:create (%wrap-degree
			      (* (+ (/ (- b r) c) 2.0) 60.0)) s v a))
		(else
		 (hsv:create (%wrap-degree
			      (* (+ (/ (- r g) c) 4.0) 60.0)) s v a))))
	(hsv:create 0 0 0 a))))

;; Creates a new HSV colour
(define (hsv:create h s v #!optional (a 1.0))
  (f32vector h s v a))

(define (hsv:h hsv)
  (f32vector-ref hsv 0))

(define (hsv:s hsv)
  (f32vector-ref hsv 1))

(define (hsv:v hsv)
  (f32vector-ref hsv 2))

(define (hsv:a hsv)
  (f32vector-ref hsv 3))

(define (hsv->rgb hsv)
  (let* ((h (hsv:h hsv))
	 (s (hsv:s hsv))
	 (v (hsv:v hsv))
	 (a (hsv:a hsv))
	 (c (* v s))
	 (m (- v c))
	 (x (* c (- 1.0 (abs (- (fmod (/ h 60.0) 2) 1)))))
	 (m (- v c)))
    (cond
     ((and (>= h 0.0)
	   (<  h 60.0))  (rgb:create (+ c m) (+ x m) m a))
     ((and (>= h 60.0)
	   (<  h 120.0)) (rgb:create (+ x m) (+ c m) m a))
     ((and (>= h 120.0)
	   (<  h 180.0)) (rgb:create m (+ c m) (+ x m) a))
     ((and (>= h 180.0)
	   (<  h 240.0)) (rgb:create m (+ x m) (+ c m) a))
     ((and (>= h 240.0)
	   (<  h 300.0)) (rgb:create (+ x m) m (+ c m) a))
     ((and (>= h 300.0)
	   (<  h 360.0)) (rgb:create (+ c m) m (+ x m) a))
     (else (rgb:create m m m a)))))

(define (rgb->colour-matrix rgb)
  (let ((lst (f32vector->list rgb)))
   (apply f32vector (flatten lst lst lst lst))))

;;-------------------------------------------------------
;; Bezier
;;-------------------------------------------------------

;; Creates a new bezier curve with a minimum of two
;; control-points.
(define (bezier:create #!rest control-points)
  (when (< (length control-points) 2)
    (error "must have more than two control-points."))
  (apply list control-points))

(define (%subdivide points k)
  (if (<= k 0)
      points
      (let loop ((left  (list))
		 (right (list))
		 (step 1)
		 (points points))
	(if (< step (length points))
	    (loop (cons (car points) left)
		  (cons (list-ref points (- (length points) step)) right)
		  (+ step 1)
		  (let loop ((r (list))
			     (p points)
			     (i 0))
		    (if (< i (- (length points) step))
			(loop (cons (vect* (vect+ (car points)
						  (cadr points))
					   .5)
				    r)
			      (cdr p)
			      (+ i 1))
			(reverse r))))
	    (let ((left
		   (%subdivide (reverse (cons (car points) left))
				     (- k 1)))
		  (right
		   (%subdivide (reverse (cons (car points) right))
				     (- k 1))))
	      (append left (cdr (reverse right))))))))

(define (%forward-difference points)
  (if (or (null? points) 
	  (null? (cdr points)))
      (list)
      (cons (vect- (cadr points) (car points))
	    (%forward-difference (cdr points)))))

;; Return the point of a bezier at position n, where 
;; 0.0 is the start of the curve and 1.0 is the end.
(define (bezier:ref bezier n)
  (when (or (< n 0)
	    (> n 1))
    (error "n must be between 0 and 1."))
  (let ((l (length bezier)))
    (let loop ((points bezier) 
	       (step   1))
      (if (< step l)
	  (loop (let loop ((i 0)
			   (r (list))
			   (bezier points))
		  (if (< i (- l step))
		      (loop (+ i 1)
			    (cons (vect+ (vect* (car bezier) (- 1 n))
					 (vect* (cadr bezier) n))
				  r)
			    (cdr bezier))
		      (reverse r)))
		(+ step 1))
	  (car (reverse points))))))

;; Returns a list of points that make of a bezier curve.
;; A higher /accuracy/ results in a higher resolution
;; (more points).
(define (bezier->vects bezier accuracy)
  (%subdivide bezier accuracy))


(define (matrix:flip vertical? horzontal? matrix)
  (let* ((matrix (if vertical?  (flip-y matrix) matrix))
	 (matrix (if horzontal? (flip-x matrix) matrix)))
    matrix))

;;-------------------------------------------------------
;; Matrix
;;-------------------------------------------------------

;; Flips the matrix.
(define (matrix:flip vertical? horzontal? matrix)
  (let* ((ret (copy-mat4 matrix))
	 (ret (if vertical?
		  (f32vector-set! ret 0 (- (f32vector-ref ret 0)))
		  ret))
	 (ret (if horzontal?
		  (f32vector-set! ret 5 (- (f32vector-ref ret 5)))
		  ret)))
    ret))

;; Translate a matrix.
(define (matrix:translate vect matrix)
  (m* (translation (f32vector (vect:x vect) (vect:y vect) 0))
      matrix))

(define (matrix:scale vect matrix)
  (m* (2d-scaling (vect:x vect)
		  (vect:y vect))
      matrix))

;; Rotates a matrix around the z axis.
(define (matrix:rotate rotation matrix)
  (m* (z-rotation rotation) matrix))

;; Returns the identity matrix.
(define identity-matrix mat4-identity)

;; Multiplies matrices.
(define matrix* m*)


;;-------------------------------------------------------
;; %
;;-------------------------------------------------------

(define (%wrap-degree v)
  (if (negative? v) (+ 360 v) v))

(define (%f32vector-part v size)
  (let loop ((r (list)) (n (f32vector-length v)))
    (if (= n 0) r
	(loop (cons (subf32vector v (- n size) n) r)
	      (- n size)))))
