(declare (unit textbuffer)
         (uses primitives
	       static-tilemap
               texture
               misc))

(import srfi-1
        (chicken string)
	(chicken random)
	matchable
        vector-lib)

(define char-map
 " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(define (%string->chars str)
  (apply vector
	 (map (lambda (c) (substring-index (string c) char-map))
	      (string->list str))))

(define-record textbuffer
  dirty?
  width height
  static-tilemap
  colour
  location
  location-start-x
  texture
  camera
  text-texture
  renderer)

(define (%textbuffer:create charmap-texture w h)
  (let ((static-tilemap (static-tilemap:create w h (coord:create 1 1) #f))
	(camera (camera:create (vect:create  (/ w 2) (exact->inexact (/ h 2))) (vect:create w h)))
	(text-texture (texture:create (window:size))))
    (make-textbuffer
     #t
     w h
     static-tilemap
     (rgb:create 1 1 1)
     (coord:create 0 0)
     0
     charmap-texture
     camera
     text-texture
     (texture:renderer text-texture))))

(define (textbuffer:render textbuffer)
  (let* ((dirty? (textbuffer-dirty? textbuffer))
	 (width (textbuffer-width textbuffer))
	 (height (textbuffer-height textbuffer))
	 (target (textbuffer-text-texture textbuffer)))
    (when dirty?
      (with-target/proc
       target
       (lambda () 
	 (with-camera/proc
	  (textbuffer-camera textbuffer)
	  (lambda () 
	    (with-blending/proc
	     'trans
	     (rgb:create 1 1 1)
	     (lambda () 
	       (with-texture/proc (textbuffer-texture textbuffer)
				  (lambda ()
				    (texture:clear! (rgb:create 0 0 0 0))
				    (static-tilemap:render (textbuffer-static-tilemap textbuffer))))))))))
      (textbuffer-dirty?-set! textbuffer #f))
    (with-blending/proc 'trans (rgb:create 1 1 1)
			(lambda ()
			  ((textbuffer-renderer textbuffer))))))

(define (textbuffer:clear! textbuffer)
  (textbuffer-dirty?-set! textbuffer #t)
  (static-tilemap:clear! (textbuffer-static-tilemap textbuffer)))

(define (%textbuffer-set! textbuffer x y text-vector colour)
  (textbuffer-dirty?-set! textbuffer #t)
  (let ((tilemap (textbuffer-static-tilemap textbuffer)))
    (map (lambda (c i)
	   (static-tilemap:set! (textbuffer-static-tilemap textbuffer) (coord:create (+ x i) y)
				(list sprite: (sprite:create-from-indices (textbuffer-texture textbuffer) 16 6 (list c))
				      colour: colour)))

	 (vector->list text-vector)
	 (iota (vector-length text-vector)))))

(define (textbuffer:set-cursor! textbuffer x y)
  (let ((width (textbuffer-width textbuffer)))
    (textbuffer-location-start-x-set! textbuffer x)
    (textbuffer-location-set! textbuffer (coord:create x y))))

(define (textbuffer:newline! textbuffer)
  (let* ((location (textbuffer-location textbuffer))
	 (x (textbuffer-location-start-x textbuffer))
	 (y (coord:y location)))
    (textbuffer:set-cursor! textbuffer x (+ y 1))))

(define (textbuffer:colour! textbuffer colour)
  (textbuffer-colour-set! textbuffer colour))

(define (textbuffer:prn! textbuffer str)
  (let* ((location (textbuffer-location textbuffer))
	 (x (coord:x location))
	 (y (coord:y location))
	 (align-x (textbuffer-location-start-x textbuffer))
	 (length (string-length str))
	 (width (textbuffer-width textbuffer))
	 (height (textbuffer-height textbuffer))
	 (colour (textbuffer-colour textbuffer))
	 (start (+ (* width y) x))
	 (end (+ start length))
	 (nx (modulo end width))
	 (ny (/ (- end nx) width)))
    (if (> (+ x length) width)
	(begin (textbuffer:prn! textbuffer (substring str 0 (- width x)))
	       (textbuffer-location-set! textbuffer (coord:create align-x (+ y 1)))
	       (textbuffer:prn! textbuffer (substring str (- width x))))
	(begin (%textbuffer-set! textbuffer x (- height 1 y) (%string->chars str) colour)
	       (textbuffer-location-set! textbuffer (coord:create nx ny))))))

(define textbuffer #f)

(define (textbuffer:init w h)
  (let* ((texture (texture:load "share/charmap.png"))
	 (textbuffer (%textbuffer:create texture w h)))
    (match-lambda (('prn str) (textbuffer:prn! textbuffer str))
		  (('colour colour) (textbuffer:colour! textbuffer colour))
		  (('newline) (textbuffer:newline! textbuffer))
		  (('cursor x y) (textbuffer:set-cursor! textbuffer x y))
		  (('clear) (textbuffer:clear! textbuffer))
		  (('%render) (textbuffer:render textbuffer)))))
