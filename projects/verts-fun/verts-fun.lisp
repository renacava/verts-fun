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
            (:smooth colour))))

(defun-g fragment-shader-stage ((colour :vec3))
  (v! colour 1.0)
  ;;(v! 0.5 0.5 0.5 0)
  )

(defpipeline-g cube-pipeline ()
  :vertex (vertex-shader-stage g-pnt)
  :fragment (fragment-shader-stage :vec3))

(defun start (&key (x 400) (y 300) (title "verts-fun"))
  ;; (start-cepl :x x :y y :title title)
  (when (cepl:repl x y)
    (setf (cepl.sdl2::vsync) t)
    (window-set-title title)
    (setf (cepl:depth-test-function) #'<=)
    (setf (clear-color (cepl-context)) (vec4 0.3 0.3 0.9 1.0))
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
          :cam-rot (q:to-mat3 (q:inverse (rot *camera*)))))
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
  (setf *gpu-array* (cube-mesh-to-gpu-arrays (combine-cube-mesh-list (make-cool-chunk chunk-size chunk-height spacing))))
  (setf *buffer-stream* (make-buffer-stream (getf *gpu-array* :verts)
                                            :index-array (getf *gpu-array* :indices)))
  (step-host)
  (setf *game-stopped-p* nil))

(def-simple-main-loop play (:on-start (lambda () (init)))
  (main-loop))

(defparameter *cube-mesh-data* (list
                                  (list (vec3 0.5 -0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                                  (list (vec3 0.5 -0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                                  (list (vec3 0.5 0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                                  (list (vec3 0.5 0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                                  (list (vec3 -0.5 -0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                                  (list (vec3 -0.5 -0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                                  (list (vec3 -0.5 0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                                  (list (vec3 -0.5 0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))))

(defparameter *cube-mesh-indices* '(4 0 3 4 3 7 0 1 2 0 2 3 1 5 6 1 6 2 5 4 7 5 7 6 7 3 2 7 2 6 0 5 1 0 4 5))


(defun cube-mesh-to-gpu-arrays (cube-mesh)
  (list :verts (make-gpu-array (getf cube-mesh :verts) :element-type 'g-pnt)
        :indices (make-gpu-array (getf cube-mesh :indices) :element-type :uint)))

(defun combine-cube-mesh-list (cubes)
  (list :verts
        (loop for cube in cubes
              nconc (getf cube :verts))
        :indices
        (loop for cube in cubes
              nconc (getf cube :indices))))

(defun make-cool-chunk (width height &optional (spacing 1.0))
  (declare (fixnum width)
           (fixnum height))
  (let* ((positions (loop for x below width
                         nconc (loop for y below height
                                     nconc (loop for z below width
                                                 collect (vec3 (float x)
                                                               (float y)
                                                               (float z))))))
         (positions (remove-if-not #'block-at-pos? positions))  ;;grab only positions of blocks that aren't air
         (positions (remove-if-not #'has-empty-neighbour? positions)))  ;; of non-air blocks, grab only those that aren't surrounded by other blocks
    (loop for index below (length positions)
          collect (block-mesh-from-pos (elt positions index)
                                       :vert-start-index (* index 8)
                                       :spacing spacing))))

(defun block-at-pos? (pos)
  "Returns T if there's a block at the given pos."
  (and (< (aref pos 1) (+ 6 (* 2 (sin (* 0.5 (aref pos 0))))))
       t
       ;;(oddp (truncate (aref pos 2)))
       ;;(oddp (truncate (aref pos 1)))
       ;;(oddp (truncate (aref pos 0)))
       ;; (oddp (truncate (aref pos 2)))
       ;; (oddp (truncate (aref pos 1)))
       ))



(defun side-to-face-indices (side &optional (start-index 0))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; left right above below ahead behind
  ;; 0    1     2     3     4     5
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (declare (fixnum side))
  (mapcar (lambda (x) (+ x start-index))
          (case side
            (0 (list 5 4 7 7 6 5))
            (1 (list 3 1 2 0 1 3))
            (2 (list 6 7 3 3 2 6))
            (3 (list 5 1 0 0 4 5))
            (4 (list 6 2 1 1 5 6))
            (5 (list 7 4 0 0 3 7)))))

(defun pos-above (pos)
  (vec3 (aref pos 0)
        (+ (aref pos 1) 1.0)
        (aref pos 2)))

(defun pos-below (pos)
  (vec3 (aref pos 0)
        (- (aref pos 1) 1.0)
        (aref pos 2)))

(defun pos-left (pos)
  (vec3 (- (aref pos 0) 1.0 )
        (aref pos 1)
        (aref pos 2)))

(defun pos-right (pos)
  (vec3 (+ (aref pos 0) 1.0 )
        (aref pos 1)
        (aref pos 2)))

(defun pos-ahead (pos)
  (vec3 (aref pos 0)
        (aref pos 1)
        (+ (aref pos 2) 1.0)))

(defun pos-behind (pos)
  (vec3 (aref pos 0)
        (aref pos 1)
        (- (aref pos 2) 1.0)))

(defun has-empty-neighbour? (pos)
  "Returns T if the given pos has an empty neighbour in any of the cardinal directions."
  (not (and (block-at-pos? (pos-above pos))
            (block-at-pos? (pos-below pos))
            (block-at-pos? (pos-ahead pos))
            (block-at-pos? (pos-behind pos))
            (block-at-pos? (pos-left pos))
            (block-at-pos? (pos-right pos)))))

(let ((side-funcs (list #'pos-left #'pos-right #'pos-above #'pos-below #'pos-ahead #'pos-behind)))
  (defun get-empty-neighbours (pos)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; left right above below ahead behind
    ;; 0    1     2     3     4     5
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (loop for side-func in side-funcs
          for index fixnum from 0
          unless (block-at-pos? (funcall side-func pos))
          collect index)))

(defun offset-cube-mesh-verts-by-pos (pos &key (spacing 1.0))
  (loop for vert in *cube-mesh-data*
        collect (list
                 (vec3 (+ (aref (first vert) 0) (* (aref pos 0) spacing))
                       (+ (aref (first vert) 1) (* (aref pos 1) spacing))
                       (+ (aref (first vert) 2) (* (aref pos 2) spacing)))
                 (second vert)
                 (third vert))))

(defun block-mesh-from-pos (pos &key (vert-start-index 0) (spacing 1.0))
  "Returns an optimized block mesh based on the given pos."
  (let ((empty-sides (get-empty-neighbours pos)))
    (when empty-sides 
      (list :verts (offset-cube-mesh-verts-by-pos pos :spacing spacing)
            :indices (loop for side in empty-sides
                           nconcing (side-to-face-indices side vert-start-index))))))
