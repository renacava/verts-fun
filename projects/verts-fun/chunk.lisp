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
         ;; (positions (mapcar (lambda (pos) (vec3 (+ (aref pos 0) pos-x)
         ;;                                        (+ (aref pos 1) pos-y)
         ;;                                        (+ (aref pos 2) pos-z)))
         ;;                    positions))
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
  (make-instance 'chunk
                 :pos pos
                 :width (abs width) 
                 :height (abs height)
                 :buffer-stream (make-chunk-stream (abs width)
                                                   (abs height)
                                                   pos)))
(defmethod render ((chunk chunk))
  (map-g #'cube-pipeline
         (buffer-stream chunk)
         :now (now)
         :perspective *perspective-matrix*
         :cam-pos (pos *camera*)
         :cam-rot (q:to-mat3 (q:inverse (rot *camera*)))
         :2d-sampler *jade-sampler*
         :debug-colour (debug-colour chunk)))
