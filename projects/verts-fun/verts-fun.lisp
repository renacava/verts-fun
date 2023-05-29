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

(defun main ()
  (start)
  (loop while (not (cepl.lifecycle:uninitialized-p))
        do (sleep 0.1)))

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
  ;; (setf (resolution (current-viewport))
  ;;       (get-cepl-context-surface-resolution))
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
  ;;(decay-events)
  )

(defun init (&optional (chunk-size 8) (chunk-height 8) (spacing 1.5))
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
;; (defparameter *cube-mesh-indices-x* '(0 2 3 1 0 3))

(defun make-cube-mesh (&key (size 1.0) (pos (vec3 0.0 0.0 0.0)) (vert-start-index 0))
  (list :verts (mapcar (lambda (vert-col-uv) (list
                                              (vec3 (* size (+ (aref pos 0) (aref (first vert-col-uv) 0)))
                                                    (* size (+ (aref pos 1) (aref (first vert-col-uv) 1)))
                                                    (* size (+ (aref pos 2) (aref (first vert-col-uv) 2))))
                                              (second vert-col-uv)
                                              (third vert-col-uv)))
                       *cube-mesh-data*)
        :indices (mapcar (lambda (x) (+ x vert-start-index))
                         *cube-mesh-indices*)))

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
         ;; (positions
         ;;   (cull-invisible-block-positions positions))
         ;; (positions (mapcar (lambda (pos) (vec3 (* spacing (aref pos 0))
         ;;                                        (* spacing (aref pos 1))
         ;;                                        (* spacing (aref pos 2))))
         ;;                    positions))
         (positions (remove-if-not #'block-at-pos? positions))  ;;grab only positions of blocks that aren't air
         (positions (remove-if-not #'has-empty-neighbour? positions)) ;; of non-air blocks, grab only those that aren't surrounded by other blocks
         
         
         )
    (loop for index below (length positions)
          collect (block-mesh-from-pos (elt positions index)
                                       :vert-start-index (* index 8)
                                       :spacing spacing))
    ;; (loop for index below (length positions)
    ;;       collect (make-cube-mesh :pos (elt positions index)
    ;;                               :vert-start-index (* index 8)))
    ;; (loop for index below (length positions)
    ;;       collect (combine-side-mesh-list (construct-block-mesh-from-sides (get-empty-neighbours (elt positions index)) (* index 8))))
    ))

(defun block-at-pos-on-chunk-edge? (pos chunk-width chunk-height)
  "Returns t if the given pos corresponds to a block that's on the edge of the chunk."
  (if (or (eq (aref pos 0) 0.0)
          (eq (aref pos 0) (float (1- chunk-width)))
          (eq (aref pos 1) 0.0)
          (eq (aref pos 1) (float (1- chunk-height)))
          (eq (aref pos 2) 0.0)
          (eq (aref pos 2) (float (1- chunk-width))))
      t))

(defun cull-invisible-block-positions (positions)
  (loop for pos in positions
        nconcing (when (and (block-at-pos? pos)
                            (has-empty-neighbour? pos))
                   (list pos))))

(defun block-at-pos? (pos)
  "Returns T if there's a block at the given pos."
  (and (< (aref pos 1) (+ 6 (* 2 (sin (* 0.5 (aref pos 0))))))
       (oddp (truncate (aref pos 2)))
       (oddp (truncate (aref pos 1))))
  ;;(> (aref pos 1) 1)
  
  )

(defun has-empty-neighbour? (pos)
  "Returns T if the given pos has an empty neighbour in any of the cardinal directions."
  (not (and (block-at-pos? (pos-above pos))
            (block-at-pos? (pos-below pos))
            (block-at-pos? (pos-ahead pos))
            (block-at-pos? (pos-behind pos))
            (block-at-pos? (pos-left pos))
            (block-at-pos? (pos-right pos)))))

(defun get-empty-neighbours (pos)
  (let ((empty-neighbours (list 'left 'right 'above 'below 'ahead 'behind)))
    (when (block-at-pos? (pos-left pos))
      (setf empty-neighbours (remove 'left empty-neighbours)))
    (when (block-at-pos? (pos-right pos))
      (setf empty-neighbours (remove 'right empty-neighbours)))
    (when (block-at-pos? (pos-above pos))
      (setf empty-neighbours (remove 'above empty-neighbours)))
    (when (block-at-pos? (pos-below pos))
      (setf empty-neighbours (remove 'below empty-neighbours)))
    (when (block-at-pos? (pos-ahead pos))
      (setf empty-neighbours (remove 'ahead empty-neighbours)))
    (when (block-at-pos? (pos-behind pos))
      (setf empty-neighbours (remove 'behind empty-neighbours)))
    empty-neighbours))

(defun construct-block-mesh-from-sides (sides &optional (starting-index 0))
  "Returns a block mesh, vert index list and number of verts in the mesh, making only the faces provided."
  (let ((result))
    (dotimes (index (length sides))
      (ntack result (side-to-face (elt sides index) (+ starting-index (* 8 index)))))
    result)
  ;; (loop for index below (length sides)
  ;;       nconcing (side-to-face (elt sides index) (* 4 index)))
  )

(defun side-to-face (side &optional (start-index 0))
  (list :verts (copy-tree *cube-mesh-data*)
        :indices (mapcar (lambda (x) (+ x start-index))
                         (case side
                           ('left (list 5 4 7 7 6 5))
                           ('right (list 3 1 2 0 1 3))
                           ('above (list 6 7 3 3 2 6))
                           ('below (list 5 1 0 0 4 5))
                           ('ahead (list 6 2 1 1 5 6))
                           ('behind (list 7 4 0 0 3 7))))))

(defun combine-side-mesh-list (side-mesh-list)
  (list :verts (copy-tree *cube-mesh-data*)
        :indices (loop for side in side-mesh-list
                       nconc (getf side :indices))))

(defun block-mesh-from-sides-list (sides-list &key (vert-start-index 0))
  (combine-side-mesh-list (mapcar (lambda (side) (side-to-face side vert-start-index)) sides-list)))

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

(defun offset-mesh-by-pos (mesh pos &key (spacing 1.0))
  "Offsets the positions of the verts in the given mesh according to the given pos."
  (list
   :verts (loop for vert in (getf mesh :verts)
                collect (list
                         (vec3 (+ (aref (first vert) 0) (* (aref pos 0) spacing))
                               (+ (aref (first vert) 1) (* (aref pos 1) spacing))
                               (+ (aref (first vert) 2) (* (aref pos 2) spacing)))
                         (second vert)
                         (third vert)))
   :indices (getf mesh :indices)))

(defun block-mesh-from-pos (pos &key (spacing 1.0) (vert-start-index 0))
  "Returns an optimized block mesh based on the given pos."
  (let ((empty-sides (get-empty-neighbours pos)))
    (when empty-sides (offset-mesh-by-pos (block-mesh-from-sides-list empty-sides :vert-start-index vert-start-index) pos :spacing spacing)))
  )
