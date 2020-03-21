(declare (unit pixmap)
         (uses primitives
               texture))

(import srfi-4)

(define-record pixmap
  width
  height
  data)

;; Gives a new pixmap.
(define (pixmap:create width height)
  (make-pixmap width height (make-u32vector (* width height) 0 #t)))

;; Converts a rgb-object to an unisgned byte.
(define rgb->u8
  (foreign-lambda* unsigned-integer ((f32vector rgb)) "
	unsigned int ret = 0;
	unsigned char r = (unsigned char)rgb[0]*255;
	unsigned char g = (unsigned char)rgb[1]*255;
	unsigned char b = (unsigned char)rgb[2]*255;
	unsigned char a = (unsigned char)rgb[3]*255;
	ret = (a<<24)|(r<<16)|(g<< 8)|b;
	C_return(ret);"))

;; Sets a pixel in a pixmap to a certian colour.
(define (pixmap:set! pixmap coord rgb)
  (u32vector-set! (pixmap-data pixmap)
		  (+ (coord:x coord)
		     (* (coord:y coord)
			(pixmap-width pixmap)))
		  (rgb->u8 rgb)))

;; Converts a pixmap to a texture.
(define (pixmap->texture pixmap)
  (texture:create (vect:create (pixmap-width  pixmap)
			       (pixmap-height pixmap))
		  (make-locative (pixmap-data pixmap))))
