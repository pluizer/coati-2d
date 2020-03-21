(declare (unit font)
         (uses misc))

(define-record font
  filename
  size+pointer)

(define (load-font filename)
  (unless (and (ttf-was-init) (zero? (ttf-init)))
    (error "Could not init sdl-ttf:" (sdl-get-error)))
  (set-finalizer!
   (make-font filename (list))
   (lambda (font)
     (for-each (lambda (s+p)
                 (ttf-close-font (cadr s+p)))
               (font-size+pointer font)))))

;; Check if a the font is already loaded with this size, if so return the cached
;; version. Else load and cache it.
(define (%font-ptr font size)
  (let ((tmp (filter (lambda (s+p) (= (car s+p) size)) (font-size+pointer font))))
    (if (null-list? tmp)
        (let ((ptr (ttf-open-font (font-filename font) size)))
          (unless ptr (error "Could not create font:" (sdl-get-error)))
          (font-size+pointer-set! font (cons (list size ptr)
                                             (font-size+pointer font)))
          ptr)
        (cadr (car tmp)))))

;; Renders (blended) a string to a texture.
(define (string->texture string font size colour)
  (let* ((font-ptr (%font-ptr font size))
         (surface (ttf-render-text-blended
                   font-ptr string (rgb->sdl-color colour))))
    (unless surface (error "Could not render string:" (sdl-get-error)))
    (sdl-surface->texture surface)))
