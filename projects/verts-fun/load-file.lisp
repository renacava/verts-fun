(in-package :verts-fun)

(let ((alternate-folder "projects/verts-fun/"))

  (defun find-file (filename &optional contingency)
    "Attempts to find the file with the given filename, and return the path leading to it. If not found, returns given contingency if provided, else nil."
    (let ((root-file (probe-file filename))
          (final-file))
      (cond ((probe-file filename) filename)
            ((probe-file (format nil "~a~a" alternate-folder filename))
             (format nil "~a~a" alternate-folder filename))
            (t (if contingency (find-file contingency nil) nil))))
    ;; (x-if-nil (probe-file filename) (format nil "~a~a" alternate-folder filename))
    ))

(let ((formats (list ".png" ".jpg" ".jpeg" ".bmp")))
  (defun find-file-image (filename)
    "Attempts to find an image-file with the given filename."
    (let ((filename (string-split filename #\.)))
      (dolist (extension formats)
        (let* ((result (find-file (format nil "~a~a" filename extension))))
          (when result
            (return-from find-file-image result)))))
    (find-file "default.png")))

