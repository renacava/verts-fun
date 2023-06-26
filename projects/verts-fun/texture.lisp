(in-package :verts-fun)

(let ((texture-table (make-hash-table :test #'equal)))

  (flet
      ((load-from-disk (file-name)
         (progn
           (setf (gethash file-name texture-table)
                 (dirt:load-image-to-texture
                  (find-file-image file-name)))
           (gethash file-name texture-table))))
    
    (defun texture-load (file-name &optional force-load-from-disk?)
      "Tries to load and return a texture based on the given file-name, if unsuccessful, returns default texture."
      (if force-load-from-disk?
          (load-from-disk file-name)
          (x-if-nil (gethash file-name texture-table)
                    (load-from-disk file-name))))

    (defun texture-print-loaded ()
      "Prints all loaded textures to standard output."
      (hash-table-print texture-table))))

(defun sampler-from-texture (texture)
  "Makes and returns a sampler from the given loaded-texture."
  (sample texture :minify-filter :nearest-mipmap-nearest :magnify-filter :nearest))

(defun sampler-from-filename (file-name)
  "Makes and returns a sampler from the given file-name."
  (sampler-from-texture (texture-load file-name)))
