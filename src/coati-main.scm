(import coati
        (chicken command-line)
        (chicken load)
        (chicken file))

(define (default-game)
  (listen-for-event `(key-down ,key-escape)
                    (lambda (#!rest _) (coati:close)))
  (values
   (lambda (#!rest _)
     (texture:clear! (rgb:create 0 1 0))
     #t)
   (list)))

(let ((args (command-line-arguments)))
  (cond
    ((not (null? args))
     (load (car args)))
    ((file-exists? "game.scm")
     (load "game.scm"))
    (else
     (coati:init 800 600 "coati" #f)
     (coati:start default-game))))
