(declare (unit node)
	 (uses trans)
	 (uses primitives))

(use srfi-1)

(define-record node
  trans
  parent
  children
  listener-ids	
  data)

;; Creates a '''root-node''' a node without a parent.
(define (node:create-root)
  (make-node (trans:create (zero-vect))
	     #f
	     (list)
	     (list)
	     #f))

;; Spawns a new node.
(define (node:spawn! parent trans #!optional data)
  (let ((node (make-node trans parent (list) (list) data)))
   (node-children-set! parent
		       (cons node (node-children parent)))
   node))

;; Removes a node and all of its descendants.
(define (node:remove! node)
  (for-each node:remove! (node-children node))
  ;; in case node is still refenced somewhere.
  (node-parent-set! node #f)
  (node:remove-all-listeners! node)
  (when (node-parent node)
   (node-children-set! (node-parent node)
		       (remove (=? node) (node-children (node-parent node))))))

;; Change the transformation of a node.
(define (node:change! node trans)
  (node-trans-set! node trans))

;; Returns a flat list of the parent of a node (and there parent etc.).
(define (node:ancestors node)
  (let ((parent (node-parent node)))
    (if parent 
	(cons parent (node:ancestors parent))
	(list))))

;; Returns a flat list of all the children (and there children etc.)
;; of a node.
(define (node:descendants node)
  (flatten
   (let ((children (node-children node)))
     (if (null? children) (list)
	 (cons children (map node:descendants children))))))

(define node:data node-data)

;; Returns the absolute transformation matrix of a node.
(define (node:matrix node)
  (let ((ancestors (node:ancestors node)))
    (if (= (length ancestors) 1)
	(trans->matrix (node-trans node))
	(reduce matrix* (identity-matrix)
		(map (o trans->matrix node-trans)
		     (cons node ancestors))))))

;; Listeners can be attached to a node. This is handy because they
;; will automatically be removed when the node is removed.
(define (node:add-listener! node args func)
  (node-listener-ids-set! node
			  (cons (listen-for-event args func)
				(node-listener-ids node))))

;; Removes a listener from a node.
(define (node:remove-listener! node id)
  (listener:remove! id)
  (node-listener-ids-set! node (remove (=? id) (node-listener-ids node))))

;; Removes all listeners from a node.
(define (node:remove-all-listeners! node)
  (for-each listener:remove! (node-listener-ids node))
  (node-listener-ids-set! node (list)))

;; Returns or sets the optional data of a node.
(define node:data
  (getter-with-setter
   node-data
   node-data-set!))
