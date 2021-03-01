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
	      ;; assuming north mirrors south, and east mirrors west
	      ((2) (list 0 1 0 1))
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

;; sprite-map is a way to quickly give names to tiles in a tile-map image.
;; Given a list of symbols it creates four sprites for each symbol, one
;; for each direction <name>-north, <name>-east, <name>-south, <name>-west
;; counting from left to right, and top to bottom.
;; The entries in the list can themselves be a list:
;; (<name>, <frames>, <dirs>), where frames is used to create an animation
;; with the <frames>s number of tiles next to it.
;; <dirs> is used for tiles that do not need seperate directions for each
;; direction i.e. a block that looks the same in every direction, in such
;; a situation should be '1'.
;;
;; example:
;;
;; (sprite-map:create texture/size 4 3
;;         '(chair (wall 1 2) (block 1 1) (water 3 1)))
;;
;; results in this tilemap:
;;
;; +--------------+--------------+--------------+--------------+
;; |chair-north   |chair-east     chair-south   |chair-west    |
;; |              |              |              |              |
;; |              |              |              |              |
;; |              |              |              |              |
;; +--------------+--------------+--------------+--------------+
;; |wall-north    |wall-east     |block-north   |water-north-1 |
;; |wall-south    |wall-west     |block-east    |water-east-1  |
;; |              |              |block-south   |water-south-1 |
;; |              |              |block-west    |water-west-1  |
;; +--------------+--------------+--------------+--------------+
;; |water-north-2 |              |              |              |
;; |water-east-2  |              |              |              |
;; |water-south-2 |              |              |              |
;; |water-west-2  |              |              |              |
;; +--------------+--------------+--------------+--------------+
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
