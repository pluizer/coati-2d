(declare (unit scene)
	 (uses node
	       trans))

(define-record scene
  spawn-callback	;; (lambda (node) ...)
  change-callback	;; (lambda (node) ...)
  remove-callback	;; (lambda (node) ...)
  root-node)

(define (scene:create spawn-callback change-callback remove-callback)
  (make-scene
   spawn-callback
   change-callback
   remove-callback
   (node:create-root)))

(define scene:root-node scene-root-node)

(define (scene:spawn! scene parent trans #!optional data)
  (let ((node (node:spawn! parent trans data)))
    ((scene-spawn-callback scene) node)
    node))

(define (scene:change! scene node trans)
  (let ((trans (if (trans-change? trans)
		   (trans-change->trans (node-trans node) trans)
		   trans)))
    (node:change! node trans)
    (let ((change-callback (scene-change-callback scene)))
      (for-each (lambda (node)
		  (change-callback node))
		(cons node (node:descendants node))))))

(define (scene:remove! scene node)
  (let ((remove-callback (scene-remove-callback scene)))
    (for-each (lambda (node)
		(remove-callback node))
	      (cons node (node:descendants node))))
  (node:remove! node))
