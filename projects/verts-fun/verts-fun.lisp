;;;; verts-fun.lisp

(in-package #:verts-fun)
(load "utilities.lisp")
(defvar *buffer-stream* nil)
(defvar *gpu-array* nil)
(defvar *perspective-matrix* nil)
(defvar *cam-pos* (v! 0 0 0))
(defvar *cam-rot* (q:identity))

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
  ;;(v! colour 1.0)
  (v! 0.5 0.5 0.5 0)
  )

(defpipeline-g cube-pipeline ()
  :vertex (vertex-shader-stage g-pnt)
  :fragment (fragment-shader-stage :vec3))

(defun now ()
  (* 0.0005 (get-internal-real-time)))

(defun get-viewport-resolution ()
  "Returns a vec2 of x/y viewport resolution"
  (resolution (current-viewport)))

(defun get-cepl-context-surface-resolution ()
  (surface-resolution (current-surface (cepl-context))))

(defun update-viewport-perspective-matrix (&key (fov 90) (near-clip 0.001) (far-clip 300))
  (setf *perspective-matrix*
        (rtg-math.projection:perspective (x (get-viewport-resolution))
                                         (y (get-viewport-resolution))
                                         (float near-clip)
                                         (float far-clip)
                                         (float fov))))

(defun draw ()
  (clear)
  (step-host)
  (setf (resolution (current-viewport))
        (get-cepl-context-surface-resolution))
  (update-viewport-perspective-matrix)
  (setf *cam-pos* (v! (* 2 (+ 4 (* 5 (sin (* 2 (now))))))
                      (* 2 (+ 6 (* 5 (sin (* 1.5 (now))))))
                      (* 1 (+ 30 (sin (* 2 (now)))))))
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

(defun init (&optional (chunk-size 16) (chunk-height 16) (spacing 1.5))
  (when t
    (when *buffer-stream*
      (free *buffer-stream*))
    (when (getf *gpu-array* :verts)
      (free (getf *gpu-array* :verts)))
    (when (getf *gpu-array* :indices)
      (free (getf *gpu-array* :indices)))
    (setf *gpu-array* (cube-mesh-to-gpu-arrays (combine-cube-mesh-list (make-cool-chunk chunk-size chunk-height spacing))))
    ;;(setf *gpu-array* (cube-mesh-to-gpu-arrays (make-cube-mesh)))
    (setf *buffer-stream* (make-buffer-stream (getf *gpu-array* :verts)
                                              :index-array (getf *gpu-array* :indices)))
    ))

(def-simple-main-loop play (:on-start (lambda () (init 16 16)))
  (draw))

;; i think these are wrong. the depth is all messed up, i tried revesing each triplet but that didn't work.
;; check online for a cube mesh and see its vert-indices list
(defparameter *cube-mesh-indices* '(0 1 2 0 2 3 4 5 6 4 6 7 8 9 10 8 10 11 12 13 14 12 14 15 16 17 18 16 18 19 20
                                    21 22 20 22 23))
;; (defparameter *cube-mesh-indices* '(2 1 0 3 2 0 6 5 4 7 6 4 10 9 8 11 10 8 14 13 12 15 14 12 18 17 16 19 18 16 22
;;                                     21 20 23 22 20))
(defparameter *cube-mesh-data* (list
                                (list (vec3 -0.5 -0.5 0.5) (vec3 0.0 0.0 1.0) (vec2 0.0 1.0))
                                (list (vec3 0.5 -0.5 0.5) (vec3 0.0 0.0 1.0) (vec2 1.0 1.0))
                                (list (vec3 0.5 0.5 0.5) (vec3 0.0 0.0 1.0) (vec2 1.0 0.0))
                                (list (vec3 -0.5 0.5 0.5) (vec3 0.0 0.0 1.0) (vec2 0.0 0.0))
                                (list (vec3 0.5 -0.5 -0.5) (vec3 0.0 0.0 -1.0) (vec2 0.0 1.0))
                                (list (vec3 -0.5 -0.5 -0.5) (vec3 0.0 0.0 -1.0) (vec2 1.0 1.0))
                                (list (vec3 -0.5 0.5 -0.5) (vec3 0.0 0.0 -1.0) (vec2 1.0 0.0))
                                (list (vec3 0.5 0.5 -0.5) (vec3 0.0 0.0 -1.0) (vec2 0.0 0.0))
                                (list (vec3 -0.5 -0.5 -0.5) (vec3 -1.0 0.0 0.0) (vec2 0.0 1.0))
                                (list (vec3 -0.5 -0.5 0.5) (vec3 -1.0 0.0 0.0) (vec2 1.0 1.0))
                                (list (vec3 -0.5 0.5 0.5) (vec3 -1.0 0.0 0.0) (vec2 1.0 0.0))
                                (list (vec3 -0.5 0.5 -0.5) (vec3 -1.0 0.0 0.0) (vec2 0.0 0.0))
                                (list (vec3 0.5 -0.5 0.5) (vec3 1.0 0.0 0.0) (vec2 0.0 1.0))
                                (list (vec3 0.5 -0.5 -0.5) (vec3 1.0 0.0 0.0) (vec2 1.0 1.0))
                                (list (vec3 0.5 0.5 -0.5) (vec3 1.0 0.0 0.0) (vec2 1.0 0.0))
                                (list (vec3 0.5 0.5 0.5) (vec3 1.0 0.0 0.0) (vec2 0.0 0.0))
                                (list (vec3 -0.5 0.5 0.5) (vec3 0.0 1.0 0.0) (vec2 0.0 1.0))
                                (list (vec3 0.5 0.5 0.5) (vec3 0.0 1.0 0.0) (vec2 1.0 1.0))
                                (list (vec3 0.5 0.5 -0.5) (vec3 0.0 1.0 0.0) (vec2 1.0 0.0))
                                (list (vec3 -0.5 0.5 -0.5) (vec3 0.0 1.0 0.0) (vec2 0.0 0.0))
                                (list (vec3 -0.5 -0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 0.0 1.0))
                                (list (vec3 0.5 -0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 1.0))
                                (list (vec3 0.5 -0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                                (list (vec3 -0.5 -0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 0.0 0.0))))

(defparameter *cube-mesh-data-x* (list
                                  (list (vec3 0.5 -0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                                  (list (vec3 0.5 -0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                                  (list (vec3 0.5 0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                                  (list (vec3 0.5 0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                                  (list (vec3 -0.5 -0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                                  (list (vec3 -0.5 -0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                                  (list (vec3 -0.5 0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))
                                  (list (vec3 -0.5 0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))))

(defparameter *cube-mesh-indices-x* '(4 0 3 4 3 7 0 1 2 0 2 3 1 5 6 1 6 2 5 4 7 5 7 6 7 3 2 7 2 6 0 5 1 0 4 5))
;; (defparameter *cube-mesh-indices-x* '(0 2 3 1 0 3))

(defun make-cube-mesh (&key (size 1.0) (pos (vec3 0.0 0.0 0.0)) (vert-start-index 0))
  (list :verts (mapcar (lambda (vert-col-uv) (list
                                              (vec3 (* size (+ (aref pos 0) (aref (first vert-col-uv) 0)))
                                                    (* size (+ (aref pos 1) (aref (first vert-col-uv) 1)))
                                                    (* size (+ (aref pos 2) (aref (first vert-col-uv) 2))))
                                              (second vert-col-uv)
                                              (third vert-col-uv)))
                       *cube-mesh-data-x*)
        :indices (mapcar (lambda (x) (+ x vert-start-index))
                         *cube-mesh-indices-x*)))

(defun cube-mesh-to-gpu-arrays (cube-mesh)
  (list :verts (make-gpu-array (getf cube-mesh :verts) :element-type 'g-pnt)
        :indices (make-gpu-array (getf cube-mesh :indices) :element-type :uint)))

(defun combine-cube-meshes (cube1 cube2)
  (list :verts (nconc (getf cube1 :verts)
                      (getf cube2 :verts))
        :indices (nconc
                  (getf cube1 :indices)
                  (getf cube2 :indices))))

(defun combine-cube-mesh-list (cubes)
  (list :verts
        (loop for cube in cubes
              nconc (getf cube :verts))
        :indices
        (loop for cube in cubes
              nconc (getf cube :indices))))

(defun make-cool-cubes (how-many &optional (spacing 1.0))
  (let ((pos (vec3 0f0 0f0 0f0))) 
    (combine-cube-mesh-list
     (mapcar (lambda (x) (make-cube-mesh
                          :pos (vec3 (+ (* x spacing) (aref pos 0))
                                     0f0
                                     0f0)))
             (range how-many)))))

(defun make-cool-chunk (width height &optional (spacing 1.2))
  (declare (fixnum width)
           (fixnum height))
  (let* ((positions (loop for x below width
                         nconc (loop for y below height
                                     nconc (loop for z below width
                                                 collect (vec3 (float x)
                                                               (float y)
                                                               (float z))))))
         (positions
           (cull-invisible-block-positions positions width height))
         (positions (mapcar (lambda (pos) (vec3 (* spacing (aref pos 0))
                                              (* spacing (aref pos 1))
                                              (* spacing (aref pos 2))))
                            positions))
         )
;;    positions
    (loop for index below (length positions)
          collect (make-cube-mesh :pos (elt positions index)
                                  :vert-start-index (* index 8;;4;;24
                                                       )))
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

(defun cull-invisible-block-positions (positions chunk-width chunk-height)
  (loop for pos in positions
        nconcing (when ;; (block-at-pos-on-chunk-edge? pos chunk-width chunk-height)
                   (and (not (query-pos pos)) ;; when block at pos should be invisible
                        (query-pos (pos-above pos))) ;; and block at pos above should be visible
                   (list pos))))

(defun query-pos (pos)
  "Returns T if the pos points to a location that has a block that should be visible."
  (> (aref pos 1) (+ 6 (* 2 (sin (* 0.5 (aref pos 0)))))))

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

(defun pos-surrounded-by-visible? (pos)
  "Returns t if the given block is surrounded by visible blocks."
  (and (query-pos (pos-above pos))
       (query-pos (pos-below pos))
       (query-pos (pos-left pos))
       (query-pos (pos-right pos))
       (query-pos (pos-ahead pos))
       (query-pos (pos-behind pos))))
