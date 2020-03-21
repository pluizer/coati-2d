;;;; coati.import.scm - GENERATED BY CHICKEN 5.2.0 -*- Scheme -*-

(##sys#with-environment
  (lambda ()
    (scheme#eval '(import-syntax scheme chicken.base chicken.foreign))
    (##sys#register-compiled-module
      'coati
      'coati
      (scheme#list)
      '()
      (scheme#list
        (scheme#cons
          'with-camera
          (syntax-rules
            ()
            ((_ camera body ...)
             (with-camera/proc camera (lambda () body ...)))))
        (scheme#cons
          'with-blending
          (syntax-rules
            ()
            ((_ type colour body ...)
             (with-blending/proc 'type colour (lambda () body ...)))))
        (scheme#cons
          'with-target
          (syntax-rules
            ()
            ((_ texture body ...)
             (with-target/proc texture (lambda () body ...)))))
        (scheme#cons
          'with-texture
          (syntax-rules
            ()
            ((_ texture body ...)
             (with-texture/proc texture (lambda () body ...))))))
      (scheme#list))))

;; END OF FILE
