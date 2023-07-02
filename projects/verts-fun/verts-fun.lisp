;;;; verts-fun.lisp

(in-package #:verts-fun)
;;(load "utilities.lisp")
(defvar *buffer-stream* nil)
(defvar *gpu-array* nil)
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

         (colour (vec3 (* (mod (aref pos 0) (+ 1.2 (sin (* 0.15 now)))) (+ 1.0 (sin (* 2 now))))
                       (* (mod (aref pos 1) (+ 1.2 (sin (* 0.05 now)))) (+ 1.0 (sin (* 3 now))))
                       (* (mod (aref pos 2) (+ 1.2 (sin (* 0.1 now)))) (+ 1.0 (cos (* 1 now))))
                       ))
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
                                (2d-sampler :sampler-2d))
  ;;(v! colour 1.0)
  (let* ((uv (vec2 (aref uv 0)
                   (+ (aref uv 1) now)))
         (result (texture 2d-sampler uv))
         (result (vec4 1.0;;(aref result 0)
                       0.0;;(+ (aref result 1) (* 0.2 (sin (* 3 now))))
                       0.0;;(aref result 2)
                       (aref result 3))))
    result))

(defpipeline-g cube-pipeline ()
  :vertex (vertex-shader-stage g-pnt)
  :fragment (fragment-shader-stage :vec3 :vec2))

(defun start (&key (x 400) (y 300) (title "verts-fun"))
  (when (cepl:repl x y)
    (play :start)))

(defun stop ()
  (unless (or (cepl.lifecycle:shutting-down-p)
              (cepl.lifecycle:uninitialized-p))
    (cepl:quit)))

(defun window-title-set (title)
  (setf (cepl:surface-title (cepl:current-surface)) (format nil "~a" title)))

(defun now ()
  (* (/ 0.5 internal-time-units-per-second) (get-internal-real-time)))

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
      (setf last-time current-time)
      ;; (print (format nil "delta-seconds = ~a~%" delta-seconds))
      ;; (print (format nil "framerate = ~a~%" framerate))
      (setf *fps* framerate
            *delta* delta-seconds))))

(defun main-loop ()
  "Contains calls to all things that should occur once per frame, like drawing."
  (when (or (cepl.lifecycle:shutting-down-p)
            (cepl.lifecycle:uninitialized-p))
    (play :stop)
    (return-from main-loop))
  (draw)
  (calculate-fps))

(defparameter *my-texts*
  (loop for text-index below 3
        collect (make-instance 'text
                               :text (format nil "~a" text-index)
                               :pos (v! (- text-index 1) -0.8)
                               :scale 0.2)))

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
    (map-g #'cube-pipeline
           *buffer-stream*
           :now (now)
           :perspective *perspective-matrix*
           :cam-pos (pos *camera*)
           :cam-rot (q:to-mat3 (q:inverse (rot *camera*)))
           :2d-sampler *text-sampler*)

    (without-depth
      (render *my-texts*)))
    
  (swap)
  (step-host)
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

(defparameter *default-blending-params* (make-blending-params))

(defun init (&optional (chunk-size 96) (chunk-height 16) (spacing 1.0))
  (when *buffer-stream*
    (free *buffer-stream*))
  (when (getf *gpu-array* :verts)
    (free (getf *gpu-array* :verts)))
  (when (getf *gpu-array* :indices)
    (free (getf *gpu-array* :indices)))
  (vsync-set t)
  (window-title-set "verts-fun")
  (depth-func-set #'<=)
  ;;  (sky-colour-set (list 0.3 0.3 0.9))
  (sky-colour-set '(0 0 0))
  (text-init-sampler)
  
  (defparameter *tile-sampler* (sampler-from-filename "tiles.png"))
  (defparameter *jade-sampler* (sampler-from-filename "jade-moon.jpg"))
  ;; (defparameter *text-sampler* (sampler-from-filename "fonts/default.bmp"))
  
  (setf *gpu-array* (cube-mesh-to-gpu-arrays (combine-cube-mesh-list (make-cool-chunk chunk-size chunk-height spacing))))
  (setf *buffer-stream* (make-buffer-stream (getf *gpu-array* :verts)
                                            :index-array (getf *gpu-array* :indices)))
  (step-host)
  (setf *game-stopped-p* nil))

(def-simple-main-loop play (:on-start (lambda () (init)))
  (main-loop))





