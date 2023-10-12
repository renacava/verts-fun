;;;; verts-fun.lisp

(in-package #:verts-fun)
(defvar *perspective-matrix* nil)
(defparameter *fps* 1)
(defparameter *delta* 1)
(defparameter *camera* (make-instance 'camera))
(defparameter *game-stopped-p* t)

(defun-g vertex-shader-stage ((vert g-pnt)
                              &uniform (now :float)
                              (perspective :mat4)
                              (cam-pos :vec3)
                              (cam-rot :mat3))
  (let* ((pos (pos vert))
         (colour (vec3 0.0 0.0 0.0))
         (pos (- pos cam-pos))
         (pos (* cam-rot pos)))
    (values (* perspective (v! pos 1.0))
            (:smooth colour)
            (tex vert)
            )))

(defun-g fragment-shader-stage ((colour :vec3)
                                (uv :vec2)
                                &uniform
                                (now :float)
                                (2d-sampler :sampler-2d)
                                (debug-colour :vec4))
  (let* ((uv (vec2 (aref uv 0)
                   (+ (aref uv 1) 0)))
         (result (texture 2d-sampler uv))
         (result (vec4 (aref debug-colour 0)
                       (aref debug-colour 1)
                       (aref debug-colour 2)
                       (aref result 0)
                       )))
    result))

(defpipeline-g cube-pipeline ()
  :vertex (vertex-shader-stage g-pnt)
  :fragment (fragment-shader-stage :vec3 :vec2))

(let ((default-width 400)
      (default-height 300))
  (defun start (&key (x default-width) (y default-height) (title "verts-fun"))
    (setf default-width x
          default-height y)
    (when (cepl:repl x y)
      (play :start)))

  (defun stop ()
    (let ((dimensions (cepl:viewport-dimensions (cepl:current-viewport))))
      (setf default-width (first dimensions))
      (setf default-height (second dimensions)))
    (unless (or (cepl.lifecycle:shutting-down-p)
                (cepl.lifecycle:uninitialized-p))
      (cepl:quit))))

(defun window-title-set (title)
  "Sets the title of the window to the given title"
  (setf (cepl:surface-title (cepl:current-surface)) (format nil "~a" title)))

(defun now ()
  "Returns the current internal real time in seconds. Maybe half of it. I'm not sure."
  (* (/ 1.0 internal-time-units-per-second) (get-internal-real-time)))

(defun get-viewport-resolution ()
  "Returns a vec2 of x/y viewport resolution"
  (resolution (current-viewport)))

(defun get-cepl-context-surface-resolution ()
  (surface-resolution (current-surface (cepl-context))))

(defun update-viewport-perspective-matrix (&key (fov 90) (near-clip 0.01) (far-clip 3000))
  (setf *perspective-matrix*
        (rtg-math.projection:perspective (x (get-viewport-resolution))
                                         (y (get-viewport-resolution))
                                         (float near-clip)
                                         (float far-clip)
                                         (float fov))))

(defun get-internal-real-time-seconds ()
  (float (/ (get-internal-real-time) internal-time-units-per-second)))

(let ((frame 0)
      (stepper (temporal-functions:make-stepper (temporal-functions:seconds 1)))
      (last-time (get-internal-real-time-seconds)))
  (defun calculate-fps ()
    (let* ((current-time (get-internal-real-time-seconds))
           (delta-seconds (- current-time last-time))
           (framerate (truncate (/ 1.0 (if (>= 0 delta-seconds)
                                           0.1
                                           delta-seconds)))))
      (setf last-time current-time)      (setf *fps* framerate
            *delta* delta-seconds))))

(defun depth-func-set (&optional (depth-func #'<=))
  "Sets the opengl depth-test function to the given depth-func."
  (setf (cepl:depth-test-function) depth-func))

(defun depth-func-get ()
  "Returns the current depth function."
  (cepl:depth-test-function))

(defmacro without-depth (&body body)
  "Runs the given body after disabling depth-testing, then re-enables it."
  `(let ((prior-func (depth-func-get)))
     (depth-func-set nil)
     ,@body
     (depth-func-set prior-func)))


(defun main-loop ()
  "Contains calls to all things that should occur once per frame, like drawing."
  ;; (unless (boundp 'my-texture)
  ;;   (defparameter my-texture (cl-soil:create-ogl-texture my-pixels my-surface-width my-surface-height)))
  ;;(gl:tex-image-2d :texture-2d 0 :rgba my-surface-width my-surface-height 0 :rgba :unsigned-byte my-pixels)
  (when (or (cepl.lifecycle:shutting-down-p)
            (cepl.lifecycle:uninitialized-p))
    (play :stop)
    (return-from main-loop))
  (draw)
  (calculate-fps)
  (step-host))

;; (defparameter *my-texts*
;;   (list
;;    (loop for text-index below 3
;;          collect (make-instance 'text
;;                                 :text (format nil "~a" text-index)
;;                                 :pos (v! (- (random 2.0) 1.0)
;;                                          (- (random 1.5) 1.0))
;;                                 :scale 0.2))))

(defun draw ()
  "Called one per-frame, this is where everything is drawn."
  (when *game-stopped-p*
    (return-from draw))
  (clear)  
  (update-camera *camera*)
  (setf (resolution (current-viewport))
        (get-cepl-context-surface-resolution))
  (update-viewport-perspective-matrix)

  (with-blending *default-blending-params*
    ;; (render (list *my-chunk*
    ;;               *my-chunk2*))
    (render *chunks*)
    (without-depth
      ;;(render *my-texts*)
      ))
    
  (swap)
  (decay-events))

(defun vsync-set (bool)
  "Turns vsync on/off, if given bool is t/nil."
  (setf (cepl.sdl2::vsync) bool))

(defun sky-colour-set (&optional (colour (list 0.3 0.3 0.9)))
  "Sets the sky-colour to the given colour."
  (setf (clear-color (cepl-context))
        (vec4 (float (first colour))
              (float (second colour))
              (float (third colour))
              1.0)))

(defparameter *default-blending-params* (make-blending-params))

(let ((default-view-distance 8)
      (default-chunk-size 8))
  (defun init (&optional (view-distance default-view-distance) (chunk-size default-chunk-size))
    (setf *game-stopped-p* nil
          default-view-distance view-distance
          default-chunk-size chunk-size)
    
          (when (boundp '*my-chunk*)
            (free (buffer-stream *my-chunk*))
            (setf *my-chunk* nil))
          (when (boundp '*chunks*)
            (mapcar (lambda (chunk)
                      (free (buffer-stream chunk))
                      (setf chunk nil))
                    *chunks*))
          (sb-ext:gc)
          (vsync-set t)
          (window-title-set "verts-fun")
          (depth-func-set #'<=)
          (sky-colour-set '(0 0 0))
          (text-init-sampler)
          (sdl2-game-controller-db:load-db)
          (defparameter *tile-sampler* (sampler-from-filename "tiles.png"))
          (defparameter *jade-sampler* (sampler-from-filename "jade-moon.jpg"))

          (defparameter *my-chunk* (make-chunk (vec3 0.0 0.0 0.0) 8 8))
          (defparameter *my-chunk2* (make-chunk (vec3 8.0 0.0 0.0) 8 8))
          (defparameter *chunks* (mapcar (lambda (xz)
                                           (make-chunk (v! (first xz) 0.0 (second xz)) chunk-size 8))
                                         (chunk-make-positions-from-location view-distance (pos *camera*) chunk-size)
                                         ;;(chunk-make-positions view-distance)
                                         ))
          (step-host)))

(def-simple-main-loop play (:on-start (lambda () (init)))
  (main-loop))

