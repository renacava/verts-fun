(in-package :verts-fun)

;; (defun text-default-atlas-texture ()
;;   "Loads the default font as an image, returning "
;;   (texture-load "fonts/silkscreen.bmp"))

(let ((char-indices (make-hash-table :size 128 :test #'equal))
      (index-chars (make-hash-table :size 128 :test #'equal))
      (char-indices-initialised? nil))
  (flet ((char-index (char)
           (let ((result (gethash char char-indices)))
             (x-if-nil result 3;; \# is the missing-char character i'm using. 
                 )))
         (index-char (index)
           (let ((result (gethash index index-chars)))
             (x-if-nil result (char-index 3)))))
    (defun text-init-char-indices ()
      "Sets up the char-indices of all characters based on the default character atlas bmp file in the fonts folder."
      (let* ((alphabet (string-to-list " !\"#$%€'()*+,-./0123456789:;<=>?@abcdefghijklmnopqrstuvwxyz[\\]^_`ABCDEFGHIJKLMNOPQRSTUVWXYZ{|}"))
             (indices (range (length alphabet))))
        (mapcar (lambda (char index)
                  (setf (gethash char char-indices) index)
                  (setf (gethash index index-chars) char))
                alphabet
                indices))
      (setf char-indices-initialised? t))
    
    (defun text-print-char-indices ()
      "Prints out the char-indices table."
      (hash-table-print char-indices))
    
    (defun text-char-to-uv (char)
      "Returns a vector of #(u v), for the UV coordinates of the given char in the default texture atlas, assuming 256x256 image with 16x16 characters."
      (let ((char-index (char-index char))
            (chars-per-row 16))
        (when char-index
          (vector
           (mod char-index chars-per-row)
           (truncate (/ char-index chars-per-row))))))

    (defun text-init-sampler ()
      "Creates and binds the sampler for accessing the text atlas, to *text-sampler*. Returns *text-sampler*."
      (unless char-indices-initialised?
        (text-init-char-indices))
      (defparameter *text-sampler* (sampler-from-filename "fonts/default.png")))

    (defun text-mesh-from-char (char)
      "Returns 2 values to represent a 2D Rect mesh with uv's aligned to show the given char"
      (let ((char-index (char-index char)))
        (multiple-value-bind (verts indices)
            (rect-mesh :u-start (/ 1.0 16)
                       :v-start (/ 1.0 16)
                       :u-end (/ 1.0 4)
                       :v-end (/ 1.0 4))
          (values verts indices)))
      )))
