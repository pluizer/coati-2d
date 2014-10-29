(declare (unit node)
	 (uses trans))

(use 2d-primitives)

(define-record node
  trans
  parent
  children
  data)

(define (node:create-root)
  (make-node (trans:create (zero-vect))
	     #f
	     (list)
	     #f))

(define (node:spawn parent trans #!optional data)
  (let ((node (make-node trans parent (list) data)))
   (node-children-set! parent
		       (cons node (node-children parent)))
   node))

(define (node:remove node)
  (for-each node:remove (node-children node))
  ;; in case node is still refenced.
  (node-parent-set! node #f)
  (node-children-set! (node-parent node)
		      (remove (lambda (child)
				(eq? child node))
			      (node-children (node-parent node)))))

(define (node:ancestors node)
  (let ((parent (node-parent node)))
    (if parent 
	(cons parent (node:ancestors parent))
	(list))))
