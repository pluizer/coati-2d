(declare (unit shader))

(use (prefix opengl-glew gl::)
     (prefix gl-utils gl::))

(define-record shader
  vertex-source fragment-source
  uniforms attributes
  program-id)

(define (shader:create vertex-source fragment-source
		       uniforms attributes)
  (make-shader vertex-source fragment-source
	       uniforms attributes
	       #f))

(define (shader:id shader)
  (if (shader-program-id shader) 
      (shader-program-id shader)
      (let ((id
	     (gl::make-program 
	      (list (gl::make-shader gl::+vertex-shader+ 
				     (shader-vertex-source shader))
		    (gl::make-shader gl::+fragment-shader+
				     (shader-fragment-source shader))))))
	(shader-program-id-set! shader id)
	id)))

(define shader:attributes shader-attributes)

(define shader:uniforms shader-uniforms)

;;; Default shader
(define default-shader
  (shader:create 
;; vertex source
#<<END
#version 120
attribute vec2 vertex; 
attribute vec2 coord; 
attribute vec4 colour;
varying vec2 f_coord;
varying vec4 f_colour;
uniform mat4 viewmatrix; 
uniform mat4 projection;
uniform vec4 u_colour;

void main()
{ 
	gl_Position = projection * viewmatrix * vec4(vertex, 0, 1);
	f_colour = colour * u_colour;
	f_coord = coord;
}
END
;; fragment source
#<<END
#version 120
uniform sampler2D texture; 
varying vec2 f_coord; 
varying vec4 f_colour;
//out vec4 fragment; 

void main()
{ 
	gl_FragColor = texture2D(texture, f_coord.st) * f_colour; 
}
END
;; uniforms
'((projection (float32: mat4))
  (viewmatrix (float32: mat4))
  (u_colour   (float32: vec4)))
;; attributes
'((vertex (float32: 2))
  (coord  (float32: 2))
  (colour (float32: 4)))))

;;; Solid shader
(define solid-shader
  (shader:create 
;; vertex source
#<<END
#version 120
attribute vec2 vertex; 
varying vec2 f_coord;
varying vec4 f_colour;
uniform mat4 viewmatrix; 
uniform mat4 projection;
uniform vec4 u_colour;

void main()
{ 
	gl_Position = projection * viewmatrix * vec4(vertex, 0, 1);
	f_colour = u_colour;
}
END
;; fragment source
#<<END
#version 120
uniform sampler2D texture; 
varying vec4 f_colour;
//out vec4 fragment; 

void main()
{ 
	gl_FragColor = f_colour; 
}
END
;; uniforms
'((projection (float32: mat4))
  (viewmatrix (float32: mat4))
  (u_colour   (float32: vec4)))
;; attributes
'((vertex (float32: 2)))))
