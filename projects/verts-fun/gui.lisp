(in-package :verts-fun)

(defmacro render-mesh2d-macro (pipeline-func mesh2d &rest args)
  `(map-g ,pipeline-func
          (buffer-stream ,mesh2d)
          :perspective *perspective-matrix*
          :offset (normalise-to-screen (resolve (offset ,mesh2d)))
          :scale (float (scale ,mesh2d))
          :colour (colour ,mesh2d)
          ,@args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEXTURED RECT SHADER

(defun-g gui-textured-vert-shader ((vert g-pt)
                          &uniform
                          (perspective :mat4)
                          (offset :vec2)
                          (scale :float))
  (let* ((pos (pos vert))
         (offset (/ offset scale))
         (pos (vec3 (+ (aref pos 0)
                       (aref offset 0))
                    (+ (aref pos 1)
                       (aref offset 1))
                    (aref pos 2)))
         (pos (* pos scale))
         (colour (vec3 1.0 1.0 1.0)))
    (values (* perspective (v! (aref pos 0) (aref pos 1) -1.0 1.0))
            (tex vert))))

(defun-g gui-textured-frag-shader ((uv :vec2)
                                   &uniform
                                   (2d-sampler :sampler-2d)
                                   (colour :vec4)
                                   (tint-strength :float))
  (let* ((result (texture 2d-sampler uv))
         (grayscale (/ (+ (aref result 0) (aref result 1) (aref result 2)) 3))
         (tinted-result (vec4 (* grayscale (aref colour 0))
                              (* grayscale (aref colour 1))
                              (* grayscale (aref colour 2))
                              (aref result 3)))
         (result (vec4 (aref result 0)
                       (aref result 1)
                       (aref result 2)
                       (aref result 3)))
         (final-result (mix result tinted-result tint-strength))
         )
    final-result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEXT RECT SHADER

(defun-g gui-text-vert-shader ((vert g-pt)
                               &uniform
                               (perspective :mat4)
                               (offset :vec2)
                               (scale :float))
  (let* ((pos (pos vert))
         (offset (/ offset scale))
         (pos (vec3 (+ (aref pos 0)
                       (aref offset 0))
                    (+ (aref pos 1)
                       (aref offset 1))
                    (aref pos 2)))
         (pos (* pos scale))
         (colour (vec3 1.0 1.0 1.0)))
    (values (* perspective (v! (aref pos 0) (aref pos 1) -1.0 1.0))
            (tex vert))))

(defun-g gui-text-frag-shader ((uv :vec2)
                               &uniform
                               (2d-sampler :sampler-2d)
                               (colour :vec4))
  (let* ((result (texture 2d-sampler uv))
         (result (vec4 (aref colour 0)
                       (aref colour 1)
                       (aref colour 2)
                       (aref result 0))))
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FLAT RECT SHADER

(defun-g gui-flat-vert-shader ((vert g-pt)
                                   &uniform
                                   (perspective :mat4)
                                   (offset :vec2)
                                   (scale :float))
  (let* ((pos (pos vert))
         (offset (/ offset scale))
         (pos (vec3 (+ (aref pos 0)
                       (aref offset 0))
                    (+ (aref pos 1)
                       (aref offset 1))
                    (aref pos 2)))
         (pos (* pos scale))
         (colour (vec3 1.0 1.0 1.0)))
    (values (* perspective (v! (aref pos 0) (aref pos 1) -1.0 1.0)))))

(defun-g gui-flat-frag-shader (&uniform
                               (colour :vec4))
  (vec4 (aref colour 0)
        (aref colour 1)
        (aref colour 2)
        1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PIPELINES

(defpipeline-g gui-textured-pipeline ()
  :vertex (gui-textured-vert-shader g-pt)
  :fragment (gui-textured-frag-shader :vec2))

(defpipeline-g gui-flat-pipeline ()
  :vertex (gui-flat-vert-shader g-pt)
  :fragment (gui-flat-frag-shader))

(defpipeline-g gui-text-pipeline ()
  :vertex (gui-text-vert-shader g-pt)
  :fragment (gui-text-frag-shader :vec2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLASSES

(defclass obj2d (moduclass)
  ((offset
    :initarg :offset
    :initform (v! 0.0 0.0)
    :accessor offset)))

(defclass mesh2d (obj2d)
  ((buffer-stream
    :initarg :buffer-stream
    :accessor buffer-stream)
   (colour
    :initarg :colour
    :initform (v! 1.0 1.0 1.0 1.0)
    :accessor colour)
   (scale
    :initarg :scale
    :initform 1.0
    :accessor scale)))

(defclass rect2d (mesh2d)
  ((width
    :initarg :width
    :initform 1.0
    :accessor width)
   (height
    :initarg :height
    :initform 1.0
    :accessor height)))

(defclass texture-rect2d (rect2d)
  ((sampler
    :initarg :sampler
    :initform *jade-sampler*
    :accessor sampler)
   (tint-strength
    :initarg :tint-strength
    :initform 0.0
    :accessor tint-strength)))

(defclass text2d (rect2d)
  ((text
    :initarg :text
    :initform "text"
    :accessor text)
   (sampler
    :initarg :sampler
    :initform nil
    :accessor sampler)))

(defmethod change-text ((obj text2d) text)
  (let* ((sampler (text-make-sampler text))
         (dimensions (texture-base-dimensions (sampler-texture sampler)))
         (width (first dimensions))
         (height (second dimensions)))
    (setf (slot-value obj 'sampler) sampler
          (slot-value obj 'width) width
          (slot-value obj 'height) height
          (slot-value obj 'buffer-stream) (make-rect-buffer-stream (/ width 256) (/ height 64)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTRUCTORS

(defmethod initialize-instance :after ((rect2d rect2d) &key (width 1.0) (height 1.0))
  (setf (slot-value rect2d 'width) width
        (slot-value rect2d 'height) height
        (slot-value rect2d 'buffer-stream) (make-rect-buffer-stream width height)))

(defmethod initialize-instance :after ((obj text2d) &rest initargs)
  (change-text obj (or (slot-value obj 'text)
                       "none")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; METHODS

(defun make-rect-buffer-stream (width height)
  (let* ((width (half width))
         (height (half height))
         (verts (make-gpu-array (list (list (v! (- width) (- height) 0.0) (v! 0 1))
                                      (list (v! (+ width) (- height) 0.0) (v! 1 1))
                                      (list (v! (+ width) (+ height) 0.0) (v! 1 0))
                                      (list (v! (- width) (+ height) 0.0) (v! 0 0)))
                                :element-type 'g-pt))
         (indices (make-gpu-array (list 0 1 2 3 0 2)
                                  :element-type :uint)))
    (make-buffer-stream verts :index-array indices :retain-arrays t)))

(defmethod render ((mesh2d mesh2d))
  (render-mesh2d-macro #'gui-flat-pipeline
                       mesh2d))

(defmethod render ((texture-rect2d texture-rect2d))
  (render-mesh2d-macro #'gui-textured-pipeline
                       texture-rect2d
                       :2d-sampler (sampler texture-rect2d)
                       :tint-strength (tint-strength texture-rect2d)))

(defmethod render ((text2d text2d))
  (render-mesh2d-macro #'gui-text-pipeline
                       text2d
                       :2d-sampler (sampler text2d)))

(let ((text-samplers (make-hash-table :test #'equal)))
  (defun text-make-sampler (text-string)
    "Returns 3 values: a sampler for the given text-string, the text-surface's width, the text surface's height, both divided by the font-size."
    (or (gethash text-string text-samplers)
        (progn
          (sdl2-ttf:init)
          (let* ((font-size 8)
                 (font (sdl2-ttf:open-font (find-file-font "silkscreen.ttf") font-size))
                 (font-surface (sdl2:convert-surface-format (sdl2-ttf:render-text-solid font text-string 255 255 255 0) :rgba8888))
                 (pixel-data (sdl2:surface-pixels font-surface))
                 (surface-size (multiple-value-list (sdl2-ttf:size-text font text-string)))
                 (surface-width (first surface-size))
                 (surface-height (second surface-size))
                 (pixel-vector (cffi:foreign-array-to-lisp pixel-data (list :array :uint8 surface-height (* surface-width 4))))
                 (texture (cepl:make-texture pixel-vector :element-type :uint8)))
            (setf (gethash text-string text-samplers)
                  (sample texture :minify-filter :nearest :magnify-filter :nearest))))))

  (defmethod (setf text) (text (text2d text2d))
    (change-text text2d text)))
