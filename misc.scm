(declare (unit misc))

(define (keyword->symbol k)
  (string->symbol (keyword->string k)))

;; Returns a function that checks if the value passed
;; to it is the same as last time.
(define (make-change-check)
  (let* ((old-value #f))
    (lambda (#!rest values)
      (if (equal? values old-value)
	  #f
	  (begin (set! old-value values)
		 #t)))))

;;; FPS utils

(define (fps-counter:create thunk)
  (let ((epoch (current-milliseconds))
	(count 0))
    (lambda ()
      (let ((now (current-milliseconds))
	    (ret count))
	(set! count (+ count 1))
	(when (> (- now epoch) 1000)
	  (set! epoch now)
	  (set! count 0)
	  (thunk (+ ret 1)))))))

(define (fps-warning:create min func)
  (let ((warned #f))
    (lambda (count)
      (if warned
	  (when (>= count min)
	    (set! warned #f)
	    (func (sprintf "fps: ~a fine again!" count)))
	  (when (< count min)
	    (set! warned #t)
	    (func (sprintf "fps: ~a too low!" count))))
      (func (sprintf "fps: ~a" count)))))

(define (pair lst)
  (reverse
   (let loop ((lst lst) (r (list)))
     (if (null? lst) r
	 (loop (cddr lst) 
	       (cons (cons (car lst) (cadr lst))
		     r))))))

