(in-package :verts-fun)

(let ((alternate-folder "projects/verts-fun/"))

  (defun find-file (filename)
    "Attempts to find the file with the given filename, and return the path leading to it."
    (x-if-nil (probe-file filename) (format nil "~a~a" alternate-folder filename))))

