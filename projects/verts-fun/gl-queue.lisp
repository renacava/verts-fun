(let ((gl-queue (list)))
  (defmacro gl-queue-add (&body body)
    "Queues the given body for execution while gl-appropriate."
    (ntack gl-queue `(lambda () ,@body)))

  (defun gl-queue-resolve ()
    "Resolves all bodies queued for gl-appropriate execution."
    (mapcar #'funcall gl-queue)
    (setf gl-queue (list))))
