(declare (unit font)
         (uses misc))

(use srfi-1
     sdl-base
     sdl-ttf)

(define-record font
  filename
  size
  pointer)

(define %loaded-fonts (list))


(define (load-font filename size)
  ;; Make sure sdl-ttf was initialised.
  (unless (and (ttf-was-init) (zero? (ttf-init)))
    (error "Could not initialise sdl-ttf:" (sdl-get-error) ;; SDL error? egg has no TTF error.
           ))
  ;; Load font, unless it was already loaded.
  (let ((fonts (filter (lambda (font)
                         (and (equal? filename (font-filename font))
                              (= size (font-size font))))
                       %loaded-fonts)))
    (if (null-list? fonts)
        (set-finalizer!
         (let ((pointer (ttf-open-font filename size)))
           (unless pointer
             (error (sprintf "Could not open font ~a:" filename) (sdl-get-error)))
           (let ((font (make-font filename size pointer)))
             (set! %loaded-fonts (cons font %loaded-fonts))
             font))
         (lambda (x)
           (ttf-close-font (font-pointer x))
           (set! %loaded-fonts (remove (=? x equal?) %loaded-fonts))))
        (font-pointer (car fonts)))))
