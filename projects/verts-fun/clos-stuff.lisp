(defun clos-instance-get-slots (instance &key (direct-slots-only nil))
  "Returns a list of symbols representing the slots of the given instance's class."
  (mapcar #'sb-mop:slot-definition-name (funcall (if direct-slots-only
                                                     #'sb-mop:class-direct-slots
                                                     #'sb-mop:class-slots)
                                                 (class-of instance))))
