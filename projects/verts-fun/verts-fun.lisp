;;;; verts-fun.lisp

(in-package #:verts-fun)
(load "utilities.lisp")
(defvar *buffer-stream* nil)
(defvar *gpu-array* nil)
(defvar *perspective-matrix* nil)
(defvar *cam-pos* (v! 0 0 0))
(defvar *cam-rot* (q:identity))
(defparameter *fps* 0)



(defun-g vertex-shader-stage ((vert g-pnt)
                              &uniform (now :float)
                              (perspective :mat4)
                              (cam-pos :vec3)
                              (cam-rot :mat3))
  (let* ((id gl-instance-id)
         (now (+ now id))
         (pos (pos vert))
        ;; (pos (+ pos (v! (* 3 (sin now)) (* 3 (cos now)) (+ -5 (sin (* 5.0 now))))))
         (colour (+ (pos vert) (v! 0.5 0.5 0.5)))
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
  (cepl:repl x y)
  (window-set-title title)
  (setf (cepl:depth-test-function) #'<=)
  (play :start))

(defun stop ()
  (play :stop)
  (cepl:quit))

(defun window-set-title (title)
  (setf (cepl:surface-title (cepl:current-surface)) (format nil "~a" title)))

(defun now ()
  (* 0.0005 (get-internal-real-time)))

(defun get-viewport-resolution ()
  "Returns a vec2 of x/y viewport resolution"
  (resolution (current-viewport)))

(defun get-cepl-context-surface-resolution ()
  (surface-resolution (current-surface (cepl-context))))

(defun update-viewport-perspective-matrix (&key (fov 90) (near-clip 0.01) (far-clip 300))
  (setf *perspective-matrix*
        (rtg-math.projection:perspective (x (get-viewport-resolution))
                                         (y (get-viewport-resolution))
                                         (float near-clip)
                                         (float far-clip)
                                         (float fov))))

(let ((frame 0)
      (stepper (temporal-functions:make-stepper (temporal-functions:seconds 1))))
  (defun calculate-fps ()
    (incf frame 1)
    (when (funcall stepper)
      (setf *fps* frame
            frame 0))))

(defun main-loop ()
  (draw)
  (calculate-fps))

(defun draw ()
  (clear)
  (step-host)
  (setf (resolution (current-viewport))
        (get-cepl-context-surface-resolution))
  (update-viewport-perspective-matrix)
  (setf *cam-pos* (v! (* 2 (+ 2 (* 2 (sin (* 2 (now))))))
                      (* 2 (+ 2 (* 2 (sin (* 1.5 (now))))))
                      (* 1 (+ 20 (sin (* 2 (now)))))))
  (setf *cam-rot* (q:from-axis-angle (v! 0 1 0)
                                     (radians 0;;(* 10 (sin (* 5.0 (now))))
                                              )))
  (with-instances 1
      (map-g #'cube-pipeline
          *buffer-stream*
          :now (now)
          :perspective *perspective-matrix*
          :cam-pos *cam-pos*
          :cam-rot (q:to-mat3 (q:inverse *cam-rot*))))
  (swap))

(defun init (&optional (chunk-size 16) (chunk-height 16) (spacing 5))
  (when t
    (when *buffer-stream*
      (free *buffer-stream*))
    (when (getf *gpu-array* :verts)
      (free (getf *gpu-array* :verts)))
    (when (getf *gpu-array* :indices)
      (free (getf *gpu-array* :indices)))
    (setf *gpu-array* (cube-mesh-to-gpu-arrays (combine-cube-mesh-list (make-cool-chunk chunk-size chunk-height spacing))))
    (setf *buffer-stream* (make-buffer-stream (getf *gpu-array* :verts)
                                              :index-array (getf *gpu-array* :indices)))))

(def-simple-main-loop play (:on-start (lambda () (init 8 8 1.5)))
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

(defun make-cool-chunk (width height &optional (spacing 1.2))
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
         (positions (mapcar (lambda (pos) (vec3 (* spacing (aref pos 0))
                                                (* spacing (aref pos 1))
                                                (* spacing (aref pos 2))))
                            positions)))
    (loop for index below (length positions)
          collect (make-cube-mesh :pos (elt positions index)
                                  :vert-start-index (* index 8)))))

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
  (< (aref pos 1) (+ 6 (* 2 (sin (* 0.5 (aref pos 0)))))))

(defun has-empty-neighbour? (pos)
  "Returns T if the given pos has an empty neighbour in any of the cardinal directions."
  (or (not (block-at-pos? (pos-above pos)))
      (not (block-at-pos? (pos-below pos)))
      (not (block-at-pos? (pos-ahead pos)))
      (not (block-at-pos? (pos-behind pos)))
      (not (block-at-pos? (pos-left pos)))
      (not (block-at-pos? (pos-right pos)))))

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

(defun construct-block-mesh-from-sides (sides)
  "Returns a block mesh, vert index list and number of verts in the mesh, making only the faces provided."
  (list (loop for side in sides
         nconcing (side-to-face side))))

(defun side-to-face (side &optional (start-index 0))
  "Returns a list of verts and indices, representing the given side of a block mesh."
  (case side
    ('left (list :verts (list (list (vec3 -0.5 -0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                              (list (vec3 -0.5 -0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                              (list (vec3 -0.5 0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                              (list (vec3 -0.5 0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0)))
                 :indices (mapcar (lambda (index) (+ index start-index)) (list 2 1 3 3 1 0))))
    ('right (list :verts (list (list (vec3 0.5 -0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                               (list (vec3 0.5 -0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                               (list (vec3 0.5 0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                               (list (vec3 0.5 0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0)))
                  :indices (mapcar (lambda (index) (+ index start-index)) (list 3 0 2 2 1 0))))
    ('above (list :verts (list (list (vec3 0.5 0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                               (list (vec3 0.5 0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                               (list (vec3 -0.5 0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                               (list (vec3 -0.5 0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0)))
                  :indices (mapcar (lambda (index) (+ index start-index)) (list 2 3 0 0 3 1))))
    ('below (list :verts (list (list (vec3 0.5 -0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                               (list (vec3 0.5 -0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                               (list (vec3 -0.5 -0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                               (list (vec3 -0.5 -0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0)))
                  :indices (mapcar (lambda (index) (+ index start-index)) (list 3 2 1 1 2 0))))
    ('ahead (list :verts (list (list (vec3  0.5 -0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                               (list (vec3  0.5 0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                               (list (vec3 -0.5 -0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                               (list (vec3 -0.5 0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0)))
                  :indices (mapcar (lambda (index) (+ index start-index)) (list 1 0 3 3 0 2))))
    ('behind (list :verts (list (list (vec3  0.5 -0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                                (list (vec3  0.5 0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                                (list (vec3 -0.5 -0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                                (list (vec3 -0.5 0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0)))
                   :indices (mapcar (lambda (index) (+ index start-index)) (list 3 2 1 1 2 0))))))

(defun side-to-face (side &optional (start-index 0))
  (list :verts (copy-tree *cube-mesh-data*)
        :indices (mapcar (lambda (x) (+ x start-index))
                         (case side
                           ('left (list 7 4 5 5 6 7))
                           ('right (list 3 0 1 1 2 0))
                           ('above (list 6 7 3 3 2 6))
                           ('below (list 5 1 0 0 4 5))
                           ('ahead (list 6 2 1 1 5 6))
                           ('behind (list 7 4 0 0 3 7))))))
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
