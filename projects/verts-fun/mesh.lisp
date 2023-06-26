(in-package :verts-fun)

(flet ((make-sampler (texture)
         (sample texture
                 :minify-filter :nearest-mipmap-nearest
                 :magnify-filter :nearest)))
  (let ((name-table (make-hash-table :test #'equal))
        (mesh-table (make-hash-table :test #'equal))
        (texture-table (make-hash-table :test #'equal))
        (sampler-table (make-hash-table :test #'equal))
        )
    (defun mesh-register (buffer-stream &optional (texture (texture-load "default.png"))
                                                  (name (gensym)))
      "Registers the given buffer-stream of a mesh and its texture."
      (setf (gethash name name-table) t)
      (setf (gethash name mesh-table) buffer-stream)
      (setf (gethash name texture-table) texture)
      (setf (gethash name sampler-table) (make-sampler texture)))

    ))
