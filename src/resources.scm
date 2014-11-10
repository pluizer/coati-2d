(declare (unit resources))

(use srfi-1)

(define-record resource
  init
  free
  value)

(define %%resources
  (list))

(define (resource:add group-name name init #!optional free)
  (let ((group (assoc group-name %%resources eq?)))
    (if group
	(if (assoc name (cdr group) eq?) #f
	    (set-cdr! group (cons (cons name
					(make-resource init free #f))
				  (cdr group))))
	(set! %%resources
	      (cons (cons group-name (list (cons name
						 (make-resource init free #f))))
		    %%resources)))))

(define (resource:get group-name name)
  (let ((group (assoc group-name %%resources)))
    (if group
	(let ((resource (assoc name (cdr group))))
	  (if resource
	      (if (resource-value (cdr resource))
		  (resource-value (cdr resource))
		  (let ((loaded ((resource-init (cdr resource)))))
		    (resource-value-set! (cdr resource) loaded)
		    loaded))
	      #f))
	#f)))

(define (collect-resources group-name)
  (let ((group (assoc group-name %%resources eq?)))
    (if group
	(map cdr (cdr group))
	(list))))

(define (free-resources group-name)
  (for-each (lambda (r)
	      (when (and (resource-value r)
			 (resource-free r))
		    ((resource-free r)
		     (resource-value r))))
	    (collect-resources group-name))
  (set! %%resources
	(alist-delete! group-name %%resources eq?)))

(define (free-all-resources)
  (for-each free-resources (map car %%resources)))
