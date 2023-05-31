;;;; verts-fun.lisp

(in-package #:verts-fun)
;;(load "utilities.lisp")
(defvar *buffer-stream* nil)
(defvar *gpu-array* nil)
(defvar *perspective-matrix* nil)
;; (defvar *cam-pos* (v! 0 0 0))
;; (defvar *cam-rot* (q:identity))
(defparameter *fps* 1)
(defparameter *delta* 1)
(defparameter *camera* (make-instance 'camera))
(defparameter *game-stopped-p* t)

(defun-g vertex-shader-stage ((vert g-pnt)
                              &uniform (now :float)
                              (perspective :mat4)
                              (cam-pos :vec3)
                              (cam-rot :mat3))
  (let* ((id gl-instance-id)
         (now (+ now id))
         (pos (pos vert))

         (colour (vec3 (* (mod (aref pos 0) (+ 1.2 (sin (* 0.15 now)))) (+ 1.0 (sin (* 2 now))))
                       (* (mod (aref pos 1) (+ 1.2 (sin (* 0.05 now)))) (+ 1.0 (sin (* 3 now))))
                       (* (mod (aref pos 2) (+ 1.2 (sin (* 0.1 now)))) (+ 1.0 (cos (* 1 now))))
                       ))
         (pos (- pos cam-pos))
         (pos (* cam-rot pos)))
    ;; (* perspective (v! pos 1))
    (values (* perspective (v! pos 1.0))
            (:smooth colour)
            (tex vert)
            )))

(defun-g fragment-shader-stage ((colour :vec3)
                                (uv :vec2)
                                &uniform (2d-sampler :sampler-2d))
  ;;(v! colour 1.0)
  (texture 2d-sampler uv)
  ;;(v! 0.5 0.5 0.5 0)
  )

(defpipeline-g cube-pipeline ()
  :vertex (vertex-shader-stage g-pnt)
  :fragment (fragment-shader-stage :vec3 :vec2))

(defun start (&key (x 400) (y 300) (title "verts-fun"))
  ;; (start-cepl :x x :y y :title title)
  (when (cepl:repl x y)
    ;; (setf (cepl.sdl2::vsync) t)
    ;; (window-set-title title)
    ;; (setf (cepl:depth-test-function) #'<=)
    ;; (setf (clear-color (cepl-context)) (vec4 0.3 0.3 0.9 1.0))
    (play :start)))

(defun stop ()
  (unless (or (cepl.lifecycle:shutting-down-p)
              (cepl.lifecycle:uninitialized-p))
    (cepl:quit)))

(defun window-set-title (title)
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
  (when (or (cepl.lifecycle:shutting-down-p)
            (cepl.lifecycle:uninitialized-p))
    (play :stop)
    (return-from main-loop))
  (draw)
  (calculate-fps)
  )

(defun draw ()
  (when *game-stopped-p*
    (return-from draw))
  (clear)  
  (update-camera *camera*)
  ;; (when (or (cepl.lifecycle:shutting-down-p)
  ;;           (cepl.lifecycle:uninitialized-p))
  ;;   (return-from draw))
  (setf (resolution (current-viewport))
        (get-cepl-context-surface-resolution))
  (update-viewport-perspective-matrix)
  (with-instances 1
      (map-g #'cube-pipeline
          *buffer-stream*
          :now (now)
          :perspective *perspective-matrix*
          :cam-pos (pos *camera*)
          :cam-rot (q:to-mat3 (q:inverse (rot *camera*)))
          :2d-sampler *texture-sampler*))
  (swap)
  (step-host)
  (decay-events)
  )

(defun init (&optional (chunk-size 96) (chunk-height 16) (spacing 1.0))
  (when *buffer-stream*
    (free *buffer-stream*))
  (when (getf *gpu-array* :verts)
    (free (getf *gpu-array* :verts)))
  (when (getf *gpu-array* :indices)
    (free (getf *gpu-array* :indices)))
  (setf (cepl.sdl2::vsync) t)
  (window-set-title "verts-fun")
  (setf (cepl:depth-test-function) #'<=)
  (setf (clear-color (cepl-context)) (vec4 0.3 0.3 0.9 1.0))
  (let ((moon-texture (dirt:load-image-to-texture (find-file "jade-moon.jpg"))))
    (when moon-texture
      (defparameter *texture-sampler* (sample moon-texture :minify-filter :nearest-mipmap-nearest :magnify-filter :nearest))))
  (setf *gpu-array* (cube-mesh-to-gpu-arrays (combine-cube-mesh-list (make-cool-chunk chunk-size chunk-height spacing))))
  (setf *buffer-stream* (make-buffer-stream (getf *gpu-array* :verts)
                                            :index-array (getf *gpu-array* :indices)))
  (step-host)
  (setf *game-stopped-p* nil))

(def-simple-main-loop play (:on-start (lambda () (init)))
  (main-loop))





