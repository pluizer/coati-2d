;; Helper syntax

;; Applies func to value, or if func is #f returns just the value.
(define-syntax maybe
  (syntax-rules ()
      ((_ func value)
       (if func (func value) value))))
