(define-syntax ir-test
  (ir-macro-transformer
   (lambda (exp inj _)
     (let ((body (car (cdr exp))))
       `(print ',(symbol-append (strip-syntax body) '-zooi))))))

(ir-test hallo)

(exit)
