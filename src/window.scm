(declare (unit window)
         (uses primitives
	       textbuffer))

(import (prefix epoxy gl::)
        (prefix gl-utils gl::)
        (chicken bitwise)
        (chicken format)
	(chicken time)
	nrepl
        (prefix sdl2 "sdl-")
        srfi-18)

(define %window-should-close? #f)
(define (coati:close)
  (set! %window-should-close? #t))
(define %window-size (vect:create 0 0))
(define %window #f)
(define %context #f)

(define (window:size) %window-size)

(define with-main-mutex
  (let ((main-mutex (make-mutex)))
    (lambda (proc)
      (dynamic-wind (lambda () (mutex-lock! main-mutex))
                    proc
                    (lambda () (mutex-unlock! main-mutex))))))

(define (game-loop iter-func prev-ret)
  (poll-input-events)
  (poll-events!)
  (sdl-gl-swap-window! %window)
  (with-main-mutex
   (let ((ret (apply iter-func (if (list? prev-ret) prev-ret
                                   (list prev-ret)))))
     (when (and ret (not %window-should-close?))
       (thread-sleep! 0.05)
       (textbuffer '(%render))
       (game-loop iter-func ret)))))

(define (coati:init w h title fullscreen?)
  (sdl-set-main-ready!)
  (unless (sdl-init!)
    (error "Could not initialise SDL." (sdl-get-error)))
  (sdl-gl-attribute-set! 'doublebuffer 1)
                                        ;(sdl-gl-attribute-set! 'swap-control 1)
  (let ((window (sdl-create-window! title 0 0 w h '(opengl resizable))))
    (unless window
      (error (sprintf "Could not set video mode (~sx~s:~s):" w h 32) (sdl-get-error)))
    (set! %context (sdl-gl-create-context! window))
    (set! %window window)
    (gl::enable gl::+texture-2d+)
    (gl::enable gl::+blend+)
    (gl::disable gl::+depth-test+)
    (gl::check-error))
  (set! %window-size (vect:create w h)))


;; Opens a windows a starts a ''game''.
;; Game must be a function that returns:
;; -) A function that takes any number argument -- that
;;    reperesent its internal state -- and returns a
;;    list of new values to that state.
;; Additionally it can return another value, a list of
;; values to its inital state.
;; For example ...
;; (coati:start 
;;              (lambda ()
;;                (values (lambda (position animation)
;;                          (if player-is-in-water
;;                              (list (update-position position) 'swimming)
;;                              (list (update-position position) 'walking)))
;;                        (list (vect:create 10 20) 'walking))))
(define (coati:start game)
  (set! textbuffer (textbuffer:init 50 25))
  (thread-start!
   (lambda ()
     (nrepl 123
            #:spawn (lambda ()
                      (thread-start!
                       (lambda ()
			 (nrepl-loop eval: (lambda (x) (with-main-mutex (lambda () (eval x)))))))))))

  (if %window
      (call-with-values
	  (lambda () (game))
	(lambda (f #!optional (a (list)))
	  (game-loop f a)))
      (error "call coati:init first")))
