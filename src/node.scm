(declare (unit node)
	 (uses trans)
	 (uses primitives))

(use srfi-1)

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

(define (node:spawn! parent trans #!optional data)
  (let ((node (make-node trans parent (list) data)))
   (node-children-set! parent
		       (cons node (node-children parent)))
   node))

(define (node:remove! node)
  (for-each node:remove! (node-children node))
  ;; in case node is still refenced somewhere.
  (node-parent-set! node #f)
  (node-children-set! (node-parent node)
		      (remove (lambda (child)
				(eq? child node))
			      (node-children (node-parent node)))))

(define (node:change! node trans)
  (node-trans-set! node trans))

(define (node:ancestors node)
  (let ((parent (node-parent node)))
    (if parent 
	(cons parent (node:ancestors parent))
	(list))))

(define (node:descendants node)
  (flatten
   (let ((children (node-children node)))
     (if (null? children) (list)
	 (cons children (map node:descendants children))))))

(define node:data node-data)

(define (node:matrix node)
  (let ((ancestors (node:ancestors node)))
    (if (= (length ancestors) 1)
	(trans->matrix (node-trans node))
	(reduce matrix* (identity-matrix)
		(map (o trans->matrix node-trans)
		     (cons node ancestors))))))