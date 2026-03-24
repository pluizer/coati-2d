(declare (unit tilemap)
         (uses primitives
	       camera
               shader
               sprite
               sprite-batcher
               texture
               misc))

(import srfi-1
        srfi-4
        (chicken sort)
        (chicken string)
        (chicken pathname)
        ssax
        matchable
        (prefix epoxy gl::)
        (prefix gl-math gl::))

(define-syntax maybe
  (syntax-rules ()
    ((_ func value)
     (if func (func value) value))))

(define %sorted-white (make-f32vector 16 1.0))

(define (%merge-sorted as bs)
  (cond ((null? as) bs)
        ((null? bs) as)
        ((< (caar bs) (caar as)) (cons (car bs) (%merge-sorted as (cdr bs))))
        (else (cons (car as) (%merge-sorted (cdr as) bs)))))

;; Creates a new tile map that can be rendered using tilemap:render
;; ``new-coords-callback`` will be called with the coords to be removed
;; and the coords that are being added (optional).
(define (tilemap:create #!key isometric? new-coords-callback (shader default-shader) (overscan 0))
  (let ((batcher (sprite-batcher:create shader))
	;; Rememer the last added coordinate and the width and height
	;; so that the sprite-batch does not have to be repopulated
	;; when these values haven't changed.
	(changed? (make-change-check))
	;; Cache all active coords.
	(active-coords (list))
        (sorted-batchers (vector (sprite-batcher:create shader)))
        (sorted-changed? (make-change-check))
        (sorted-tiles (list))
        (sorted-extra-depths #f))
    (let ((raw                      ; Renders the tiles using the sprite-batcher
	   (lambda (coord
		    width height
		    ;; A function that takes a coordinate and returns a tile number.
		    tile-func
                    tile-args
		    dirty?
                    projection view)

             ;; This part will load new tiles from the ``tile-func`` when needed...
	     ;; If ``coord`` ``width`` ``height`` or ``tile-func``
	     ;; changed we'll repopulate te sprite-batch.
	     (let ((repopulate? (or dirty? (changed? coord width height tile-func))))
	       (when repopulate? 
		 (let* ((coord (if isometric? (coord->isometric coord) coord))
			(hw (if isometric? (quotient width 2) 0))
			(hh (if isometric? (quotient height 2) 0))
			(coords
			 (map (lambda (x)
				(coord:create (+ (- (modulo x width) hw)
						 (coord:x coord))
					      (+ (- (quotient x width) hh)
						 (coord:y coord))))
			      ((if isometric? reverse (lambda (x) x)) (iota (* width height))))))
		   ;; Call the optional callback with the coords to be removed
		   ;; and the coords that are being added.
		   (when new-coords-callback
		     ;; Check which coords will be newly added and which are the
		     ;; ones to keep
		     (let-values (((keep new)
				   (partition (lambda (x)
						(member x active-coords)) coords)))
		       (new-coords-callback
			coord
			(filter (lambda (x) (not (member x coords))) active-coords)
			new)))
		   ;; Clear the previously added sprites and add the new ones
		   ;; (Dumbly clearing everything an reading is often
		   ;; faster than keeping track of and deleting all unneeded
		   ;; handles one by one.)
		   (sprite-batcher:clear! batcher)
		   (for-each
			(lambda (tile-coord)

			  (let ((sprite ((apply tile-func tile-args) tile-coord)))
			    ;; It is possible not to have a sprite at these coords.
			    (when sprite
			      (let* ((x (exact->inexact (- (coord:x tile-coord)
							   (coord:x coord))))
				     (y (exact->inexact (- (coord:y tile-coord)
							   (coord:y coord))))
				     (trans (trans->matrix
					     (trans:create
					      (if isometric?
						  (vect:create (+ (* x -.5)
								  (* y .5))
							       (+ (* x -.25)
								  (* y -.25)
								  ))
						  (vect:create x y))
					      ))))
			        ;; Push the tile to the batcher.
				(match sprite
				 ;; sprite with no special colour
				 ((? sprite? sprite)
				  (sprite-batcher:push! batcher sprite trans))
				 ((sprite: sprite colour: colour)
				  (sprite-batcher:push! batcher sprite trans colour)))))))
			coords)
		   (set! active-coords coords))))
	     ;; Render the sprite-batch
	     (sprite-batcher:render* batcher projection view)))

         (sorted-raw
          (lambda (coord width height tile-func tile-args dirty? extra-specs tile-texture projection view)
            (let* ((iso-coord (coord->isometric coord))
                   (hw (quotient width 2))
                   (hh (quotient height 2))
                   (repopulate? (or dirty? (sorted-changed? coord width height tile-func))))
              (when repopulate?
                (let* ((coords (map (lambda (x)
                                      (coord:create (+ (- (modulo x width) hw)
                                                       (coord:x iso-coord))
                                                    (+ (- (quotient x width) hh)
                                                       (coord:y iso-coord))))
                                    (iota (* width height)))))
                  (set! sorted-tiles
                        (sort
                         (filter-map
                          (lambda (tile-coord)
                            (let ((sprite ((apply tile-func tile-args) tile-coord)))
                              (and sprite
                                   (let* ((x (exact->inexact
                                              (- (coord:x tile-coord) (coord:x iso-coord))))
                                          (y (exact->inexact
                                             (- (coord:y tile-coord) (coord:y iso-coord))))
                                          (mat (trans->matrix
                                                (trans:create
                                                 (vect:create (+ (* x -.5) (* y .5))
                                                              (+ (* x -.25) (* y -.25))))))
                                           (depth (if isometric? (- (+ (coord:x tile-coord) (coord:y tile-coord))) (+ (coord:x tile-coord) (coord:y tile-coord)))))
                                     (match sprite
                                       ((? sprite? spr)
                                        (list depth (sprite:vertex-data spr mat) (sprite:coord-data spr) #f))
                                       ((sprite: spr colour: col)
                                        (list depth (sprite:vertex-data spr mat) (sprite:coord-data spr)
                                              (if (= (f32vector-length col) 4) (rgb->colour-matrix col) col))))))))
                          coords)
                         (lambda (a b) (< (car a) (car b)))))))
               (let* ((depth-transform (if isometric? - (lambda (x) x)))
                      (extra-entries (sort (map (lambda (e) (list (depth-transform (car e)) 'extra (cdr e))) extra-specs)
                                           (lambda (a b) (< (car a) (car b)))))
                     (new-depths (map car extra-entries))
                     (n-segments (+ (length extra-entries) 1))
                     (rebuild? (or repopulate? (not (equal? new-depths sorted-extra-depths)))))
                (when rebuild?
                  (set! sorted-extra-depths new-depths)
                  (when (< (vector-length sorted-batchers) n-segments)
                    (set! sorted-batchers
                      (list->vector
                       (append (vector->list sorted-batchers)
                               (map (lambda (_) (sprite-batcher:create shader))
                                    (iota (- n-segments (vector-length sorted-batchers))))))))
                  (let clear ((i 0))
                    (when (< i n-segments)
                      (sprite-batcher:clear! (vector-ref sorted-batchers i))
                      (clear (+ i 1))))
                  (let loop ((tiles sorted-tiles)
                             (extras extra-entries)
                             (bi 0))
                    (when (not (null? tiles))
                      (cond
                        ((or (null? extras) (< (caar tiles) (caar extras)))
                         (let* ((t (car tiles))
                                (vdata (cadr t)) (cdata (caddr t)) (col (cadddr t)))
                           (batcher:push! (sprite-batcher-batcher (vector-ref sorted-batchers bi))
                                         vdata cdata (or col %sorted-white)))
                         (loop (cdr tiles) extras bi))
                        (else
                         (loop tiles (cdr extras) (+ bi 1)))))))
                (sprite-batcher:render* (vector-ref sorted-batchers 0) projection view)
                (let render-loop ((extras extra-entries) (bi 1))
                  (when (not (null? extras))
                    ((caddr (car extras)))
                    (when tile-texture
                      (gl::bind-texture gl::+texture-2d+ (texture:texture-id tile-texture)))
                    (sprite-batcher:render* (vector-ref sorted-batchers bi) projection view)
                    (render-loop (cdr extras) (+ bi 1)))))))))

      ;; Function returned by ``tilemap:create``. Renders the map from
      ;; the ``bottom-left`` coordinate for orthogonal maps and the top-left
      ;; for isometric maps. (which is a vect not a coord so
      ;; fractions are possible).
      ;;
      ;;For isometric maps:
      ;;
      ;;               /\  ---> coordinate (0,0) at
      ;;              /  \      position (0, 0)
      ;;             /0, 0\
      ;;            /\    /\
      ;;           /  \  /  \
      ;;          /0, 1\/1, 0\
      ;;         /\    /\    /\
      ;;        /  \  /  \  /  \
      ;;       /0, 2\/1, 1\/2, 0\
      ;;      /\    /\    /\    /\
      ;;     /  \  /  \  /  \  /  \
      ;;    /0, 3\/1, 2\/2, 1\/3, 0\
      ;;   /\    /\    /\    /\    /\
      ;;  /  \  /  \  /  \  /  \  /  \
      ;; /    \/    \/    \/    \/    \
      ;;
      ;;
      ;; For orthographic maps:
      ;; +----+----+----+
      ;; |    |    |    |
      ;; |2, 0|2, 1|2, 2|
      ;; +----+----+----+
      ;; |    |    |    |
      ;; |1, 0|1, 1|1, 2|
      ;; +----+----+----+
      ;; |    |    |    |
      ;; |0, 0|0, 1|0, 2|
      ;; +----+----+----+
      ;;
      ;; ^
      ;; |__ coordinate (0, 0) at
      ;;     position (0, 0)
      (define outer
        (lambda (top-left width height trans-func tile-func tile-args dirty? projection view)
	(sprite-batcher:update! batcher)

	(let* ((x (vect:x top-left))
	       (y (vect:y top-left))
	       (fx (floor x))
	       (fy (floor y))
               )
	  (if isometric?
              (let* ((cx0 (inexact->exact fx))
                     (cy0 (inexact->exact fy))
                     (cx (if (> overscan 0) (* overscan (inexact->exact (floor (/ cx0 overscan)))) cx0))
                     (cy (if (> overscan 0) (* overscan (inexact->exact (floor (/ cy0 overscan)))) cy0)))
                (raw
                 (coord:create (+ cx overscan) (+ cy overscan))
                 (+ (* width 2) 2 (* 4 overscan))
                 (+ (* height 4) 2 (* 4 overscan))
                 tile-func
                 tile-args
                 dirty?
                 projection
                 (maybe trans-func (matrix:translate (vect:create (exact->inexact (+ fx (- cx cx0) overscan))
                                                                   (exact->inexact (+ fy (- cy cy0) overscan)))
                                                     (subf32vector view 0 16)))))
              (let* ((sx (inexact->exact (floor (- x (/ width 2)))))
                     (sy (inexact->exact (floor (- y (/ height 2)))))
                     (sx (if (> overscan 0) (* overscan (inexact->exact (floor (/ sx overscan)))) sx))
                     (sy (if (> overscan 0) (* overscan (inexact->exact (floor (/ sy overscan)))) sy)))
         (raw
               (coord:create (- sx overscan) (- sy overscan))
	       (+ width 2 (* 2 overscan))
	       (+ height 2 (* 2 overscan))
	       tile-func
               tile-args
	       dirty?
	       projection
	       (maybe trans-func (matrix:translate (vect:create (exact->inexact (- sx overscan))
                                                                (exact->inexact (- sy overscan))) (subf32vector view 0 16)))))))))

      (define sorted-outer
        (lambda (top-left width height trans-func tile-func tile-args dirty? extra-specs tile-texture projection view)
          (let* ((x (vect:x top-left))
                 (y (vect:y top-left))
                 (fx (floor x))
                 (fy (floor y))
                 (cx0 (inexact->exact fx))
                 (cy0 (inexact->exact fy))
                 (cx (if (> overscan 0) (* overscan (inexact->exact (floor (/ cx0 overscan)))) cx0))
                 (cy (if (> overscan 0) (* overscan (inexact->exact (floor (/ cy0 overscan)))) cy0))
                 (world-coord (coord:create (+ cx overscan) (+ cy overscan)))
                 (tx (exact->inexact (+ fx (- cx cx0) overscan)))
                 (ty (exact->inexact (+ fy (- cy cy0) overscan))))
            (sorted-raw
             world-coord
             (+ (* width 2) 2 (* 4 overscan))
             (+ (* height 4) 2 (* 4 overscan))
             tile-func tile-args dirty? extra-specs tile-texture projection
             (maybe trans-func (matrix:translate (vect:create tx ty)
                                                 (subf32vector view 0 16)))))))

      (vector outer sorted-outer))))

;; Renders a procedural generated tilemap from the position of a camera.
;; tile-func:
;; a function taking a coordinate and returning either:
;; - a sprite to render
;; - an alist with a sprite and a colour as in (:sprite the-sprite :colour the-colour)
;;   colour can either be an rgb:colour, or a colour matrix for each corner of the sprite.
;; Note, do not specify tile-func as an anynymous function because then it will call it
;; everytime ``tilemap:render`` is called, even though no new tiles needed to be calculated.
;; ``tile-func-args`` the arguments to apply to ``tile-func``
;; ``trans-func``: The optional transperancy function to apply.
(define (tilemap:render tilemap trans-func dirty? tile-func #!rest tile-func-args)
  (let* ((camera (current-camera))
         (projection (camera:projection camera))
         (view (camera:view camera))
         (width (inexact->exact (camera:width camera)))
         (height (inexact->exact (camera:height camera))))
    ((vector-ref tilemap 0) (camera:pos camera) width height
                            trans-func
                            tile-func tile-func-args
                            dirty?
                            projection view)))

(define (tilemap:render-sorted tilemap trans-func dirty? tile-func tile-texture extra-specs #!rest tile-func-args)
  (let* ((camera (current-camera))
         (projection (camera:projection camera))
         (view (camera:view camera))
         (width (inexact->exact (camera:width camera)))
         (height (inexact->exact (camera:height camera))))
    ((vector-ref tilemap 1) (camera:pos camera) width height
                            trans-func
                            tile-func tile-func-args
                            dirty? extra-specs tile-texture
                            projection view)))

;; **************************************
;; Tilemap .tsx loading
;; **************************************

(define (%open-tile-definition path)
  (let* ((dir        (pathname-directory path))
	 (port       (open-input-file path))
	 (sxml       (ssax:xml->sxml port '()))
	 (tileset    (cdr  (assq 'tileset (cdr sxml))))
	 (attr       (cdr  (assq '@ tileset)))
	 (tiles-w    (cadr (assq 'tilewidth attr)))
	 (tiles-h    (cadr (assq 'tileheight attr)))
	 (tilecount  (cadr (assq 'tilecount attr)))
	 (image      (cdr  (assq 'image tileset)))
	 (attr       (cdr  (assq '@ image)))
	 (width      (cadr (assq 'width attr)))
	 (height     (cadr (assq 'height attr)))
	 (source     (pathname-replace-directory (cadr (assq 'source attr)) dir))
	 (tiles      (filter (match-lambda ((x . _) (eq? x 'tile))) tileset))
	 ;; loop trough all the tiles with special properties
	 ;; for now only supports animations (without custom
	 ;; frame durations.)
	 (tiles      (map (lambda (tile)
			    (let* ((tile (cdr tile))
				   (attr (cdr (assq '@ tile)))
				   (id   (string->number (cadr (assq 'id attr))))
				   (animation (assq 'animation tile)))
			      ;; when this tile contains an animation return a list of its frames.
			      ;; TODO: Implement 'duration'.
			      (if animation
				  (list id
					(map (lambda (frame)
					       (let* ((attr (cdr  (assq '@ (cdr frame))))
						      (id   (cadr (assq 'tileid attr))))
						 (string->number id)))
					     (filter (match-lambda ((x . _) (eq? x 'frame))) (cdr animation))))
				  ;; else nothing ...
				  ;; for later, when a tile can have properties but no animation.
				  (list (list id)))))
			  tiles)))
    (list tiles-w:      (string->number tiles-w)
	  tiles-h:      (string->number tiles-h)
	  tilecount:    (string->number tilecount)
	  texture-size: (vect:create (string->number width) (string->number height))
	  source:       source
	  tiles:        tiles)))

(define (%load-tiles-from-definition #!key
				     tiles-w tiles-h tilecount
				     texture-size source
				     tiles)
  (let ((w (/ (vect:x texture-size) tiles-w))
	(h (/ (vect:y texture-size) tiles-h))
	(;; first check if this tile-id comes is defined in tiles
	 ;; if it is it means it has special properties (right now
	 ;; only animation frames). if it is use these. if the tile
	 ;; is not defined it is a regular tile, just return the id.
	 frames
	 (lambda (id)
	   (if (assq id tiles)
	       (cadr (assq id tiles))
	       (list id)))))

    ;; for every tile in the tile-map generate a sprite ...
    (map (lambda (id)
	   (list id (sprite:create-from-indices texture-size w h (frames id))))

	 (iota tilecount))))

;; Opens a simple .tsx file as saved by Tiled.
;; Right now supports simple files with animated tiles.
(define (%open-tile-file path)
  (apply %load-tiles-from-definition (%open-tile-definition path)))

;; Opens a simple .tmx file with no compression as saved by Tiled.
;; Only supports tile data in csv and doesn't support any compression.
(define (open-tile-map path)
  (let* ((dir       (pathname-directory path))
	 (port      (open-input-file path))
	 (sxml      (cdr  (ssax:xml->sxml port '())))
	 (mapp      (cdr  (assq 'map sxml)))
	 (attr      (cdr  (assq '@ mapp)))
	 (width     (cadr (assq 'width attr)))
	 (height    (cadr (assq 'height attr)))
	 (tiles-w   (cadr (assq 'tilewidth attr)))
	 (tiles-h   (cadr (assq 'tileheight attr)))
	 (tileset   (cdr  (assq 'tileset mapp)))
	 (attr      (cdr  (assq '@ tileset)))
	 (source    (pathname-replace-directory (cadr (assq 'source attr)) dir))
	 (firstgid  (string->number (cadr (assq 'firstgid attr))))
	 (layers    (filter (match-lambda ((x . _) (eq? x 'layer))) mapp))
	 (data      (map (lambda (x) (caddr (assq 'data (cdr x)))) layers))
	 (tiles     (%open-tile-file source))
	 ;; find all layers in this map.
	 (layers (apply vector
			(map
			 (lambda (x)
			   (apply vector
				  (map (lambda (x)
					 (apply vector
						(map (lambda (x)
						       ;; firstgid is the first id of the first tile. in Coati
						       ;; we always start counting tiles from 0. So we substract
						       ;; the firstgid from every tile-id.
						       ;; This means that 0 cannot be used as 'no tile' anymore
						       ;; so -1 will take over this function.
						       (let ((id (- (string->number x) firstgid)))
							 (if (assq id tiles)
							     (cadr (assq id tiles))
							     #f))) (string-split x ","))))
				       (string-split x "\n"))))
			 data))))
    (list width:   (string->number width)
	  height:  (string->number height)
	  tiles-w: (string->number tiles-w)
	  tiles-h: (string->number tiles-h)
	  layers:  layers)))
