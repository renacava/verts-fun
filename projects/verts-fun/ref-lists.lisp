(let ((bindings-table (make-hash-table :size 2048 :test #'equal)))
  (defmacro setl (symbol value)
    "Sets the value of the given symbol to the given value, for on-request evaluation via getl."
    (setf (gethash symbol bindings-table) `(lambda () ,value)))

  (defmacro getl (symbol)
    "Evaluates and returns the value of the given symbol, that was set via setl."
    (multiple-value-bind (result found?) (gethash symbol bindings-table)
      (if found?
          `(funcall ,result)
          (cerror "Continue" "Lazy-symbol ~a not bound" symbol))))

  (defmacro petl (symbol)
    "Prints the given symbol's value, as set via setl."
    (multiple-value-bind (result found?) (gethash symbol bindings-table)
      (if found?
          (format t "~a~%" result)
          (cerror "Continue" "Lazy-symbol ~a not bound" symbol))))

  (defun print-bindings-table ()
    "Prints to standard-output the hash-table containing all bindings bound via setl."
    (maphash (lambda (key value) (format t "~a: ~a~%" key value))
             bindings-table)))
