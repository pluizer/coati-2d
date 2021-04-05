(declare (unit tilemap)
         (uses primitives
               shader
               sprite-batcher
               texture
               misc))

(import srfi-1
        srfi-4
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

;; Creates a new tile map that can be rendered using tilemap:render
;; ``new-coords-callback`` will be called with the coords to be removed
;; and the coords that are being added (optional).
(define (tilemap:create #!key isometric? new-coords-callback (shader default-shader))
  (let ((batcher (sprite-batcher:create shader))
	    ;; Rememer the last added coordinate and the width and height
	    ;; so that the sprite-batch does not have to be repopulated
	    ;; when these values haven't changed.
	    (changed? (make-change-check))
	    ;; Cache all active coords.
	    (active-coords (list)))
    (let ((raw                      ; Renders the tiles using the sprite-batcher
	       (lambda (coord
		            width height
		            ;; A function that takes a coordinate and returns a tile number.
		            tile-func
                    tile-args
                    projection view)
             ;; This part will load new tiles from the ``tile-func`` when needed...
	         ;; If ``coord`` ``width`` ``height`` or ``tile-func``
	         ;; changed we'll repopulate te sprite-batch.
	         (when (changed? coord width height tile-func)
	           (let* ((coord (if isometric? (coord->isometric coord) coord))
                      ;; List of all coordinates
		              (coords
		               (map (lambda (x)
			                  (coord:create (+ (modulo x width)
					                           (coord:x coord))
					                        (+ (floor (/ x width))
					                           (coord:y coord))))
			                (iota (* width height)))))
		         ;; Check which coords will be newly added and which are the
		         ;; ones too keep
                 ;;
		         (let-values (((keep new)
			                   (partition (lambda (x)
					                        (member x active-coords)) coords)))
		           (when (or (not (null? new))
			                 (not (= (length active-coords)
				                     (length coords))))
		             ;; Call the optional callback with the coords to be removed
		             ;; and the coords that are being added.
 		             (when new-coords-callback
		               (new-coords-callback
                        coord 
                        (filter (lambda (x) (not (member x coords))) active-coords)
                        new))
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
			                  ((match-lambda
				                ;; sprite with no special colour
				                ((? sprite? sprite)
				                 (sprite-batcher:push! batcher sprite trans))
				                ((sprite: sprite colour: colour)
				                 (sprite-batcher:push! batcher sprite trans colour)))
			                   sprite)))))
		              coords)
		             (set! active-coords coords)))))
	         ;; Render the sprite-batch
	         (sprite-batcher:render* batcher projection view))))
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
      (lambda (top-left width height trans-func tile-func tile-args projection view)
	    (sprite-batcher:update! batcher)

	    (let* ((x (vect:x top-left))
	           (y (vect:y top-left))
	           (fx (floor x))
	           (fy (floor y))
               )
	      (if isometric?
              (raw
               ;; For isometric maps the top-left side of the screen/texture
               ;; is coordinate (0, 0) of the map. (When the position of the
               ;; camera is also (0, 0)
               (coord:create
		        (inexact->exact (floor (+ fx (/ width 2))))
                (inexact->exact (floor (+ fy (/ height 2)))))
               ;; We must extend the size of the tiles to render
               ;; so also the coners of screen are filled.
               ;;
               ;;    /\  /\
               ;;   /  \/  \
               ;;  /+--/\--+\
               ;; / |X/  \X| \
               ;; \ |/    \| /
               ;;  \/      \/
               ;;  /\      /\
               ;; / |\    /| \
               ;; \ |X\  /X| /
               ;;  \+--\/--+/
               ;;   \  /\  /
               ;;    \/  \/
               (+ (* width 2) 1)
	           (+ (* height 4) 1)
               tile-func
               tile-args
               projection
               (maybe trans-func (matrix:translate (vect:create fx (+ fy height)) view)))
              (raw
               ;; For orthographic maps the bottom-left side of the screen/texture
               ;; is coordinate (0, 0) of the map. (When the position of the
               ;; camera is also (0, 0).
               ;; Moving the camera by one in a direction will move the map also
               ;; by one in that direction (depending on zoom level).
               (coord:create
		        (inexact->exact (round fx))
                (inexact->exact (round fy)))
               ;; Half of the size of a tile can be offscreen. That is why we
               ;; must also render a border of 1 tile with so there won't be
               ;; an empty part on the order edge.
	           (+ width 1)
	           (+ height 1)
	           tile-func
               tile-args
	           projection
	           (maybe trans-func (matrix:translate (vect:create (- fx (/ width 2))
                                                                (- fy (/ height 2))) view)))))))))

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
(define (tilemap:render tilemap trans-func tile-func #!rest tile-func-args)
  (let* ((camera (current-camera))
         (projection (camera:projection camera))
         (view (camera:view camera))
         (width (inexact->exact (camera:width camera)))
         (height (inexact->exact (camera:height camera))))
    (tilemap (camera:pos camera) width height
             trans-func
             tile-func tile-func-args
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
