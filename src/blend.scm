(declare (unit blend))

(use extras
     (prefix opengl-glew gl::))

(define-record blend-mode
  type colour)

(define %blend-mode-stack (list (make-blend-mode
				 'normal
				 (f32vector 1 1 1 1))))

(define (%set-blend-mode! type)
  (apply gl::blend-func
	 (case type
	   ((normal)
	    (list gl::+one+ gl::+zero+))
	   ((trans)
	    (list gl::+src-alpha+ gl::+one-minus-src-alpha+))
	   ((add)
	    (list gl::+dst-color+ gl::+one-minus-dst-alpha+))
	   ((one-one)
	    (list gl::+one+ gl::+one+))
	   (else (error (sprintf "~a, no such blend-type" type))))))

(define (%current-colour)
  (blend-mode-colour (car %blend-mode-stack)))

(define (with-blending/proc type colour thunk)
  (%set-blend-mode! type)
  (set! %blend-mode-stack (cons (make-blend-mode type colour) %blend-mode-stack))
  (thunk)
  (set! %blend-mode-stack (cdr %blend-mode-stack))
  (%set-blend-mode! (blend-mode-type (car %blend-mode-stack))))
