(in-package :verts-fun)

(defun make-chunk-stream (width height pos)
  (declare (fixnum width)
           (fixnum height))
  
  (let* ((spacing 1.0)
         (pos-x (aref pos 0))
         (pos-y (aref pos 1))
         (pos-z (aref pos 2))
         (positions (loop for x below width
                          nconc (loop for y below height
                                      nconc (loop for z below width
                                                  collect (vec3 (float (+ x pos-x))
                                                                (float (+ y pos-y))
                                                                (float (+ z pos-z)))))))
         (positions (remove-if-not #'block-at-pos? positions)) ;;grab only positions of blocks that aren't air
         (positions (remove-if-not #'has-empty-neighbour? positions)) ;; of non-air blocks, grab only those that aren't surrounded by other blocks
         )
    (let ((chunk-mesh (combine-cube-mesh-list
                       (loop for index below (length positions)
                             collect (block-mesh-from-pos (elt positions index)
                                                          :vert-start-index (* index 24) ;;24 because the cube mesh we're using has 24 verts, i think
                                                          :spacing spacing)))))
      (make-buffer-stream
       (make-gpu-array (getf chunk-mesh :verts) :element-type 'g-pnt)
       :index-array (make-gpu-array (getf chunk-mesh :indices) :element-type :uint)
       :retain-arrays nil)))
  
  )

(defclass chunk (moduclass)
  ((pos
    :initarg :pos
    :initform (v! 0 0 0)
    :accessor pos)
   (width
    :initarg :width
    :initform 8
    :accessor width)
   (height
    :initarg :height
    :initform 8
    :accessor height)
   (buffer-stream
    :initarg :buffer-stream
    :accessor buffer-stream)
   (debug-colour
    :initarg :debug-colour
    :initform (v! (random 1.0)
                  (random 1.0)
                  (random 1.0)
                  1.0)
    :accessor debug-colour)))

(defun make-chunk (pos width height)
  "Returns an instance of the chunk class"
  (let ((scaled-pos (v! (* (aref pos 0) width) 0.0 (* (aref pos 2) width))))
    (make-instance 'chunk
                   :pos scaled-pos
                   :width (abs width) 
                   :height (abs height)
                   :buffer-stream (make-chunk-stream (abs width)
                                                     (abs height)
                                                     scaled-pos))))
(defmethod render ((chunk chunk))
  (map-g #'cube-pipeline
         (buffer-stream chunk)
         :now (now)
         :perspective *perspective-matrix*
         :cam-pos (pos *camera*)
         :cam-rot (q:to-mat3 (q:inverse (rot *camera*)))
         :2d-sampler *jade-sampler*;;*text-sampler*
         :debug-colour (debug-colour chunk)
         ))

(defun chunk-make-positions (radius &optional (x-offset 0) (z-offset 0))
  "Returns a list of '(x y) positions representing a square filled with positions to spawn chunks in.
A radius of 12 would return a 24x24 grid, since radius is essentially chunk view-distance."
  (let* ((chunk-positions)
         (half-radius radius)
         (radius (* 2 radius)))
    (dotimes (x radius)
      (dotimes (z radius)
        (ntack chunk-positions (list (+ x-offset (- x half-radius)) (+ z-offset(- z half-radius))))))
    (mapcar (lambda (xz) (list (truncate (first xz))
                                (truncate (second xz))))
            chunk-positions)))

(defun pos-world-to-chunk (world-position &optional (chunk-width 8))
  "Returns world-position vector converted into chunk space."
  (v! (round (/ (aref world-position 0) chunk-width))
      (round (/ (aref world-position 1) chunk-width))
      (round (/ (aref world-position 2) chunk-width))))

(defun chunk-make-positions-from-location (radius position &optional (chunk-width 8))
  "Returns a list of '(x y) positions around an area from the given position, for making chunks. position should be a vec3."
  (let* ((chunk-position (pos-world-to-chunk position chunk-width)))
    (chunk-make-positions radius (aref chunk-position 0) (aref chunk-position 2))))
