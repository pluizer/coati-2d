(declare (unit sprite-map)
         (uses sprite
               primitives))

(import matchable
	srfi-1)

(define (%faces-names name index count dirs)
  (map (lambda (y d)
	 `(,(string->symbol (string-append
			     (symbol->string d))) ,(iota count y)))
       (map (lambda (x) (+ (* x count) index))
	    (case dirs 
	      ((1) (list 0 0 0 0))
	      ((2) (list 0 0 1 1))
	      ((4) (list 0 1 2 3))
	      (else (error "dirs can only be 1, 2 or 4"))))
       '(north east south west)))

(define (%expand-default mapping)
  (map (lambda (i)
	 (if (list? i)
	     `(,(first i) 0 ,(second i) ,(third i))
	     `(,i 0 1 4)))
       mapping))

(define (%count-tiles mapping)
  (reverse (fold (lambda (l r)
		   (match-let (((name index count dirs) l)
			       (((_ p-index p-count p-dirs) . _)
				(if (null? r) '((_ 1 0 0)) r)))
		     (let ((index (+ index p-index p-count p-dirs -1)))
		       (cons `(,name ,index ,count ,dirs) r))))
		 '()
		 mapping)))

(define (%generate-mappings mapping)
  (map (match-lambda
	 ((name index count dirs)
	  `(,name . ,(list (%faces-names name index count dirs)))))
       (%count-tiles (%expand-default mapping))))

(define (sprite-map:create texture/size tiles-w tiles-h mapping
			   #!optional
			   (size (vect:create 1 1))
			   (interval (/ 1000 20)))
  (map
   (lambda (group)
     (let ((name (first group))
	   (dirs (second group)))
       `(,name ,(map (lambda (dir)
		       (let ((name (first dir))
			     (inidices (second dir)))
			 `(,name
			   ,(sprite:create-from-indices
			     texture/size tiles-w tiles-h inidices
			     size interval))))
		     dirs))))
   (%generate-mappings mapping)))

(define (sprite-map:lookup sprite-map name direction)
  (let ((a (assq name sprite-map)))
    (if a (let ((b (assq direction (second a))))
	    (if b (second b) #f))
	#f)))
