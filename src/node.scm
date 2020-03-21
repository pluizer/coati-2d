(declare (unit node)
         (uses primitives
               trans
               misc
               sprite-batcher))

(import srfi-1)

(define-record specials
  size
  on-remove
  on-change)

(define (%specials:create #!optional
			  (size (zero-vect))
			  (on-remove null-func)
			  (on-change null-func))
  (make-specials size on-remove on-change))

(define-record node
  parent
  children
  trans
  listener-ids
  dirty?	;; must vertices be calculted again?
  vertices	;; cached vertices.
  bb		;; bounding-box rect.
  data
  specials)

;; Removes a node and all of its descendants.
(define (node:remove! node)
  ((specials-on-remove (node-specials node)) node)
  (for-each node:remove! (node-children node))
  ;; in case node is still refenced somewhere.
  (node-parent-set! node #f)
  (node:remove-all-listeners! node)
  (when (node-parent node)
   (node-children-set! (node-parent node)
		       (remove (=? node) (node-children (node-parent node))))))

;; Change the transformation of a node.
(define (node:change! node trans/trans-change)
  (let ((trans (if (trans? trans/trans-change)
		   trans/trans-change
		   (trans-change->trans (node-trans node)
					trans/trans-change))))
   (for-each (lambda (node)
	       (node-dirty?-set! node #t))
	     (cons node (node:descendants node)))
   (node-trans-set! node trans)
   ((specials-on-change (node-specials node)) node trans)))

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

;; Returns the transformation of a node.
(define node:trans node-trans)

;; Returns the size of this a node.
(define (node:size node) (specials-size (node-specials node)))

;; Returns the nodes bounding box.
(define (node:bb node)
  ;; Check if a new value must be calculated.
  (when (or (node-dirty? node)
	    (not (node-bb node)))
	(node-bb-set!
	 node
	 (rect:container (node:vertices node))))
  (node-bb node))

;; Returns the absolute vertices that make up this node.
(define (node:vertices node)
  ;; Check if a new value must be calculated.
  (when (or (node-dirty? node)
	    (not (node-vertices node)))
	(let ((verts (map (lambda (vect)
			    (vect*matrix vect (node:matrix node)))
			  (let* ((size (node:size node))
				 (w (vect:x size))
				 (h (vect:y size)))
			    (polygon->vects (rect->polygon (rect:create 0 w 0 h)))))))
	  (node-dirty?-set! node #f)
	  (node-vertices-set! node verts)))
  (node-vertices node))

;; Filters ''nodes'' down those whose aabb (axis aligned bounding box)
;; collide with the aabb of ''node''.
(define (node:bb-collide? node nodes)
  (let ((a (node:bb node)))
    (filter (lambda (b)
	      (rect:intersects? a (node:bb b)))
	    nodes)))

;; Filters ''nodes'' down those that collide with ''node''.
(define (node:collide? node nodes)
  (let ((a (node:vertices node)))
    (filter (lambda (b)
	      (vects:collide? a (node:vertices b) #t))
	    ;; only check nodes that have colliding bounding boxes
	    ;; for speed.
	    (node:bb-collide? node nodes))))


;; Spawns a new node. ''specialiser'' sets the type type of the node.
;; (See section: specialisers).
;; Optionally some user-data can be given to the new node.
(define (spawn-node! specialiser parent trans #!optional data)
  (let ((node
	 (make-node parent (list) trans (list) #t #f #f data #f)))
    (node-specials-set! node (specialiser node))
    node))

;; Returns a root node. A node without a type or a parent.
(define (node:create-root)
  (make-node #f (list) (trans:create (zero-vect)) (list) #t #f #f #f
	     (make-specials (zero-vect) null-func null-func)))

;; Returns the absolute postion of a node
(define (node:position node)
  (vect*matrix (trans-position (node-trans node))
	       (node:matrix node)))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specialisers
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Node specialiser for a node that represents a simple point in 2d-space.
(define (node-point)
  (lambda (node)
    (%specials:create)))

;; Node specialiser for a node that represents a rectangle in 2d-space.
(define (node-rect size)
  (lambda (node)
    (%specials:create size)))

;; A node specialiser that represents a sprite in a sprite-batcher.
(define (sprite-node sprite-batcher sprite)
  (lambda (node)
    (let ((id (sprite-batcher:push! sprite-batcher
				    sprite
				    (node:matrix node))))
      (%specials:create (sprite:size sprite)
			;; on remove
			(lambda (node)
			  (sprite-batcher:remove! sprite-batcher id))
			;; on change
			(lambda (node trans)
			  (sprite-batcher:change! sprite-batcher id
						  (node:matrix node)
						  (trans:colour-matrix trans)
						  ))))))

;; A node specialiser that represents a light in a sprite-batcher. This
;; is simply a sprite of two times ''radius'' centred around the middle
;; with a specific colour.
(define (node-light sprite-batcher sprite radius colour)
  (lambda (node)
    (spawn-node!
     (lambda (sub)
       (let ((id (sprite-batcher:push! sprite-batcher
				       sprite
				       (node:matrix sub)
				       (rgb->colour-matrix colour)
				       )))
	 (%specials:create (vect:create (* radius 2) (* radius 2))
			   (lambda (node)
			     (sprite-batcher:remove! sprite-batcher id))
			   (lambda (node trans)
			     (sprite-batcher:change! sprite-batcher id
						     (node:matrix sub)
						     (trans:colour-matrix trans))))))
     node (trans:create (vect:create (- radius) (- radius))
			scale: (vect:create (* radius 2) (* radius 2))
			colour: colour))
    (%specials:create)))
