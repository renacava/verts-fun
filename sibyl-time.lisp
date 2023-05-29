
(defun list-time (&key real-time-ms user-run-time-us system-run-time-us
                        gc-run-time-ms processor-cycles eval-calls
                        lambdas-converted page-faults bytes-consed
                        aborted)
  (let ((total-run-time-us (+ user-run-time-us system-run-time-us)))
    (list  :real-time-ms real-time-ms
           :total-run-time-us total-run-time-us
           :user-run-time-us user-run-time-us
           :system-run-time-us system-run-time-us
           ;; (if (zerop gc-run-time-ms) 1 0)
           :gc-run-time-ms gc-run-time-ms
           ;; Round up so we don't mislead by saying 0.0 seconds of non-GC time...
           :non-gc-time (- (ceiling total-run-time-us 1000) gc-run-time-ms)
           :eval-calls eval-calls
           :lambdas-converted lambdas-converted
           :processor-cycles processor-cycles
           :page-faults page-faults
           :bytes-consed bytes-consed
           :aborted aborted)))

(defmacro get-timing (form)
  "Returns a list of time related information, having run the given form."
  `(call-with-timing #'list-time (lambda () ,form)))
