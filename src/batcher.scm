(declare (unit batcher)
         (uses chunk
               shader
               misc))

(import (chicken keyword)
        symbol-utils
        srfi-1
        srfi-4
        (chicken locative)
        (prefix epoxy gl::)
        (prefix gl-utils gl::))

(define (index-func:create mode count func)
  (let ((value (u16vector))
	(old-size 0))
    (lambda (size)
      (unless (= size old-size)
	(set! old-size size)
	(set! value
	      (apply u16vector 
		     (map inexact->exact
			  (map func (iota (* size count)))))))

      (gl::draw-elements mode (* count size) gl::+unsigned-short+
			 (make-locative value)))))

(define *triangle-mode*
  (index-func:create gl::+triangles+ 3
                     (lambda (x) x)))

(define *triangle-rect-mode* 
  (index-func:create gl::+triangles+ 6
		     (lambda (x)
		       (+ (vector-ref (vector 0 1 2 0 2 3)
				      (modulo x 6))
			  (* (floor (/ x 6)) 4)))))


(define-record %attribute
  data		; attribute's data inside a chunk-vector
  enable-func	; function to enable attribute.
  define-func	; function to define attributes value.
  push-func	; returns batch-id
  remove-func	; function to remove this attributes
		; part of a batch.
  set-func	; function to change this attributes
		; part of a batch.
  )

(define-record batcher
  program	; the shader program to use when rendering.
  count		; the number of primitives that make up one
		; batch.
  uniforms	; ((name . set-func) ...)
  attributes	; ((name . %attribute) ...)
  length	; the number of batches the batcher holds
  index-func
  )

(define (%uniform:create batcher name type #!optional location)
  (let ((type (car type))
	(size (cadr type))
	(location
	 (if location location 
	     (gl::get-uniform-location (batcher-program batcher) 
				       (symbol->string name)))))
    (cons name
	  (lambda (value)
	    ;; check if value has the correct type
	    (unless ((case type
		       ((float32:) f32vector?)
		       ((int32:)   s32vector?)) value)
	      (error (sprintf "~a: value is of wrong type, needed (~a ~a)."
			      value type size)))
	    ((case type
	       ((float32:) ;; TODO other names
		(case size
		  ((vec1) gl::uniform1fv)
		  ((vec2) gl::uniform2fv)
		  ((vec3) gl::uniform3fv)
		  ((vec4) gl::uniform4fv)
		  ((mat2) (lambda (l c v)
			    (gl::uniform-matrix2fv l c #f v)))
		  ((mat3) (lambda (l c v)
			    (gl::uniform-matrix3fv l c #f v)))
		  ((mat4) (lambda (l c v)
			    (gl::uniform-matrix4fv l c #f v)))
		  (else (error size "incorrect size for a float uniform."))))
	       ((int32:) ;; TODO other names
		(error "TODO: Integer uniform: Not implemented yet."))
	       (else (error type "illigal unirform type,  must be float/integer.")))
	     location 1 value)))))

;; generic srfi-4 vector-length function.
(define (%vector-length value)
  ((cond ((f32vector? value) f32vector-length)
	 ((f64vector? value) f64vector-length)
	 ((s8vector? value) s8vector-length)
	 ((s16vector? value) s16vector-length)
	 ((s32vector? value) s32vector-length)
	 ((u8vector? value) u8vector-length)
	 ((u16vector? value) u16vector-length)
	 ((u32vector? value) u32vector-length)
	 (error "expected a srfi-4 vector.")) value))

;; check if ''type'' is a valid attribute.
;; types can be float32: int32 etc. or (float32: 2) (int32: 4) etc. for vectors.
(define (%valid-attribute-type type)
  (or 
   ;; for vectors
   (and (list? type)
	(= (length type) 2)
	(keyword? (car type))
	(number? (cadr type))
	(>= (cadr type) 1)
	(<= (cadr type) 4))
   ;; for non-vectors
   (keyword? type)))

(define (%attribute:create batcher name type #!optional location)
  ;; check if we have a valid type.
  (unless (%valid-attribute-type type)
    (error (sprintf "~a: not a valid attribute type." type)))
  (let* ((size (if (list? type) (cadr type) 1))
	 (type (if (list? type) (car type) type))
	 ;; the size of each data chunk.
	 (chunk-size (* size (batcher-count batcher)))
	 ;; if location is set, use it, else get location from attribute name.
	 (location (or location (gl::get-attrib-location (batcher-program batcher)
							 (symbol->string name))))
	 ;; create the cunk-vector that holds the batcher's data.
	 (data (make-chunk-vector type chunk-size)))
    ;; if type is a vector, check if it has a supported type.
    (when (or (< size 1)
	      (> size 4))
      (error (sprintf "~a: not a valid array size for an attribute." size)))
    ;; check if we have a valid location.
    (unless (>= location 0)
      (error (sprintf "~a: no such attribute in program." name)))
    (let* (;; the function that enable this attribute.
	   (enable-func 
	    (lambda ()
	      (gl::enable-vertex-attrib-array location)))
	   ;; the function that defines the data of the attribute.
	   (define-func 
	     (lambda ()
	       (gl::vertex-attrib-pointer location size (gl::type->gl type)
					  #f 0 (chunk-vector:>pointer data))))
	   ;; the function used to push new data.
	   (push-func
	    (lambda (value)
	      ;; check the size of the value
	      (unless (= (%vector-length value) chunk-size)
		(error (sprintf "~a: vector is wrong size needed size: ~a." 
				value chunk-size)))
	      ;; push the data.
	      (chunk-vector:push! data value)))
	   ;; the function used to remove data.
	   (remove-func
	    (lambda (batch-id)
	      (chunk-vector:remove! data batch-id)))
	   ;; the function used to set old data to a new value.
	   (set-func
	    (lambda (batch-id value)
	      ;; check the size of the value
	      (unless (= (%vector-length value) chunk-size)
		(error (sprintf "~a: vector is wrong size needed size: ~a." 
				value chunk-size)))
	      ;; set the data
	      (chunk-vector:set! data batch-id value))))
      ;; return a key value pair where the key is the attribute's name and
      ;; the value the actual attribute.
      (cons name (make-%attribute data enable-func define-func
				  push-func remove-func set-func)))))

;; Creates a new batcher.
;; program 	: the shader program to use when rendering.
;; count	: the number of primitives that make up one
;;		  batch. For example: if batches must be
;;		  triangles, this value should be 3.
(define (batcher:create shader index-func count)
  (let ((batcher (make-batcher (shader:id shader) count (list) (list) 0
			       index-func)))
    (batcher-uniforms-set!
     batcher (map (lambda (uniform)
		    (apply %uniform:create (cons batcher uniform)))
		  (shader:uniforms shader)))
    (batcher-attributes-set!
     batcher (map (lambda (attrib) (apply %attribute:create 
					  (cons batcher attrib)))
		  (shader:attributes shader)))
    batcher))

;; Pushes attribute ''value''s into the batcher.
;; Values should be in the same order as attributes
;; are defined in ''batcher:create''
;; Returns an batch-id which can be used to remove this
;; batch from the batcher.
(define (batcher:push! batcher #!rest values)
  (batcher-length-set! batcher (+ (batcher-length batcher) 1))
  (let ((attribs (map cdr (batcher-attributes batcher))))
    (unless (= (length attribs)
	       (length values))
	    (error "must have the same number of values as attributes."))
    (car
     ;; returned values should all be the same
     (map (lambda (attrib value)
	    ((%attribute-push-func attrib) value))
	  attribs values))))

;; Remove an batch-id from the batch.
(define (batcher:remove! batcher batch-id)
  (batcher-length-set! batcher (- (batcher-length batcher) 1))
  (for-each (lambda (attrib)
	      ((%attribute-remove-func attrib) batch-id))
	    (map cdr (batcher-attributes batcher))))

;; Change one attribute value of a batch-id.
;; attribute name should be a keyword describing the name of the attribute.
;; example "vertex" becomes vertex:
(define (batcher:change!-1 batcher batch-id attribute-name value)
  (let ((pair (assoc (keyword->symbol attribute-name) (batcher-attributes batcher))))
    (if pair ((%attribute-set-func (cdr pair)) batch-id value)
	(error attribute-name "no such attribute."))))

(define (batcher:change! batcher batch-id #!rest name+values)
  (for-each (lambda (pair)
	      (batcher:change!-1 batcher batch-id
				 (car pair)
				 (cdr pair)))
	    (pair name+values)))

;; Render the batch to the current framebuffer.
;; A value for each uniform defined in ''batcher:create''
;; should be given, in the same order as defined.
(define (batcher:render batcher #!rest uniform-values)
  (let ((uniform-setters (map cdr (batcher-uniforms batcher)))
	(attribs (map cdr (batcher-attributes batcher))))

    (gl::use-program (batcher-program batcher))
    ;; set uniform values.
    (for-each (lambda (uniform-setter value)
		(uniform-setter value))
	      uniform-setters uniform-values)
    ;; enable attributes.
    (for-each (lambda (attrib)
    		((%attribute-enable-func attrib)))
    	      attribs)
    ;; set attribute values.
    (for-each (lambda (attrib)
    		((%attribute-define-func attrib)))
    	      attribs)
    ;; draw all batches.
    ((batcher-index-func batcher) (batcher-length batcher))))

(define (batcher:clear! batcher)
  (batcher-length-set! batcher 0)
  (for-each (lambda (attrib)
	      (chunk-vector:clear! (%attribute-data attrib)))
	    (map cdr (batcher-attributes batcher))))
