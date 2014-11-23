(declare (unit events))

(use srfi-1
     data-structures)

(define %events (list))
(define %listeners (list))

(define-record listener
  args
  func
  id)

(define (send-event #!rest args)
  (set! %events (cons args %events)))

(define (listen-for-event args func)
  (let ((id (gensym)))
    (set! %listeners (cons (make-listener args func id) %listeners))
    id))

(define (listener:remove! id)
  (set! %listeners
	(remove (lambda (listener)
		  (eq? (listener-id listener)
		       id))
		%listeners)))

;; Returns a list of arguments to apply to
;; the listeners callback if listener matches
;; event. Returns #f otherwhise.
(define (%listener-match? listener event)
  (let loop ((a (listener-args listener))
	     (b event))
    (cond ((null? a) b)
	  ((null? b) #f)
	  (else (if (equal? (car a) (car b))
		    (loop (cdr a) (cdr b))
		    #f)))))

(define (%poll-events events listeners)
  (append-map (lambda (listener)
		(let loop ((events events))
		  (if (null? events)
		      ;; Keep events that did not have a matching event ...
		      (list listener)
		      (let ((args (%listener-match? listener
						    (car events))))
		       (if args
			   (let ((r (apply (listener-func listener) args)))
			     (if r
				 ;; ... also keep listeners that returned true ...
				 (loop (cdr events))
				 ;; ... but remove those that returned false.
				 (list)))
			(loop (cdr events)))))))
	      listeners))

(define (poll-events!)
  (set! %listeners
	(%poll-events %events %listeners))
  (set! %events (list)))
