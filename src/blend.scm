(declare (unit blend))

(use extras
     (prefix opengl-glew gl::))

(define %blend-mode-stack '(normal))

(define (set-blend-mode! mode)
  (apply gl::blend-func
	 (case mode
	   ((normal)
	    (list gl::+one+ gl::+zero+))
	   ((trans)
	    (list gl::+src-alpha+ gl::+one-minus-src-alpha+))
	   ((add)
	    (list gl::+dst-color+ gl::+one-minus-dst-alpha+))
	   ((one-one)
	    (list gl::+one+ gl::+one+))
	   (else (error (sprintf "~a, no such blend-mode" mode))))))

(define (with-blend-mode/proc mode thunk)
  (set-blend-mode! mode)
  (set! %blend-mode-stack (cons mode %blend-mode-stack))
  (thunk)
  (set! %blend-mode-stack (cdr %blend-mode-stack))
  (set-blend-mode! (car %blend-mode-stack)))
