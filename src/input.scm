(declare (unit input))

(use (prefix glfw3 fw::))

(fw::key-callback
 (lambda (window key scancode action mods)
   (cond
    ((eq? action fw::+press+)
     (send-event 'key-down key mods))
    ((eq? action fw::+release+)
      (send-event 'key-up key mods)))))

;; TODO
