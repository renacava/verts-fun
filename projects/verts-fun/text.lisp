(in-package :verts-fun)

(let ((char-indices (make-hash-table :size 128 :test #'equal))
      (index-chars (make-hash-table :size 128 :test #'equal))
      (text-meshes (make-hash-table :size 128 :test #'equal))
      (text-buffers (make-hash-table :size 128 :test #'equal))
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

    (defun text-print-text-meshes ()
      "Prints out the text-meshes table."
      (hash-table-print text-meshes))
    
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
      ;;(sdl2-ttf:init)
      (unless char-indices-initialised?
        (text-init-char-indices))
      (sdl2-ttf:init)
      (let* ((text-string "i love my renski so very much she is an integral part of my wellbeing")
             (font (sdl2-ttf:open-font (find-file-font "silkscreen.ttf") 100))
             (font-surface (sdl2:convert-surface-format (sdl2-ttf:render-text-solid font text-string 255 255 255 255) :rgba8888))
             (pixel-data (sdl2:surface-pixels font-surface))
             (surface-size (multiple-value-list (sdl2-ttf:size-text font text-string)))
             ;; (text-texture (cl-soil:create-ogl-texture pixel-data (first surface-size) (second surface-size)))
             )
        ;;(setq my-texture (cepl.sdl2-image:sdl-surface-to-texture font-surface :uint8))
        (setq my-surface font-surface)
        (setq my-pixels pixel-data)
        (setq my-surface-width (first surface-size))
        (setq my-surface-height (second surface-size))
        (setq my-format (sdl2:surface-format font-surface))
        (sdl2-image:save-png font-surface "text-export.png")
        ;;(setq my-text-texture text-texture)
        )
      (defparameter *text-sampler* (sampler-from-filename "fonts/default.png"))
      )

    (defun text-mesh-from-char (char)
      "Returns a list of 2 values to represent a 2D Rect mesh with uv's aligned to show the given char"
      (multiple-value-bind (result found?) (gethash char text-meshes)
        (if found?
            (values (first result) (second result))
            (let* ((char-index (char-index char))
                   (chars-per-line 16)
                   (step (/ 1.0 chars-per-line))
                   (test-index char-index)
                   (test-row (truncate (/ test-index chars-per-line)))
                   (test-column (mod test-index chars-per-line)))
              (multiple-value-bind (verts indices)
                  (rect-mesh :u-start (* test-column step)
                             :v-start (* test-row step)
                             :u-end (+ step (* test-column step))
                             :v-end (+ step (* test-row step)))
                (setf (gethash char text-meshes) (list verts indices)))
              (gethash char text-meshes)))))

    ;; (let ((buffer-func (cache-func
    ;;                     (lambda (char)
    ;;                       (let ((mesh (text-mesh-from-char char)))
    ;;                         (make-buffer-stream (first mesh)
    ;;                                             :index-array (second mesh)
    ;;                                             :retain-arrays t))))))
    ;;   (defun text-buffer-stream-from-char (char)
    ;;     (funcall buffer-func char)))
    
    (defun text-buffer-stream-from-char (char)
      (multiple-value-bind (result found?) (gethash char text-buffers)
        (if found?
            result
            (let ((mesh (text-mesh-from-char char)))
              (setf (gethash char text-buffers)
                    (make-buffer-stream (first mesh)
                                        :index-array (second mesh)
                                        :retain-arrays t))
              (gethash char text-buffers)))))
    )

  (text-init-char-indices))

(defclass text (moduclass)
  ((pos
    :initarg :pos
    :initform (v! 0.0 0.0)
    :accessor pos)
   (scale
    :initarg :scale
    :initform 1.0
    :accessor scale)
   (text
    :initarg :text
    :initform "default"
    :accessor text)))

(defgeneric render (obj)
  (:documentation
   "Renders the given object when called within the draw-loop."))

(defmethod render (obj)
  nil)

(defmethod render ((obj list))
  (mapcar #'render obj))

(defmethod render ((obj text))
  
  (map-g #'gui-pipeline
         (text-buffer-stream-from-char (aref (text obj) 0))
         :perspective *perspective-matrix*
         :2d-sampler *text-sampler*
         :offset (pos obj)
         :scale (scale obj)))
