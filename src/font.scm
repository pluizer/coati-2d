(declare (unit font)
         (uses misc))

(use srfi-1
     srfi-4
     sdl-base
     sdl-ttf)

(define-record font
  filename
  size+pointer)

(define (load-font filename)
  (unless (and (ttf-was-init) (zero? (ttf-init)))
    (error "Could not init sdl-ttf:" (sdl-get-error)))
  (make-font filename (list)))

(define (%font-ptr font size)
  (let ((tmp (filter (lambda (s+p) (= (car s+p) size)) (font-size+pointer font))))
    (if (null-list? tmp)
        (let ((ptr (ttf-open-font (font-filename font) size)))
          (unless ptr (error "Could not create font:" (sdl-get-error)))
          (font-size+pointer-set! font (cons (list size ptr)
                                             (font-size+pointer font)))
          ptr)
        (cadr (car tmp)))))


(define (string->texture string font size colour)
  (let* ((font-ptr (%font-ptr font size))
         (surface (ttf-render-text-blended
                   font-ptr string (rgb->sdl-color colour))))
    (unless surface (error "Could not render string:" (sdl-get-error)))
    (sdl-surface->texture surface)))
