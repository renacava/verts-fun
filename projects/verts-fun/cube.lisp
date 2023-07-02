(in-package :verts-fun)

(let ((+cube-vert-array+ (list
                          (list (vec3 -0.5 -0.5 0.5) (vec3 0.0 0.0 1.0) (vec2 0.0 1.0))    ;; 0   left-bottom-back
                          (list (vec3 0.5 -0.5 0.5) (vec3 0.0 0.0 1.0) (vec2 1.0 1.0))     ;; 1   right-bottom-back
                          (list (vec3 0.5 0.5 0.5) (vec3 0.0 0.0 1.0) (vec2 1.0 0.0))      ;; 2   right-top-back
                          (list (vec3 -0.5 0.5 0.5) (vec3 0.0 0.0 1.0) (vec2 0.0 0.0))     ;; 3   left-top-back
                          (list (vec3 0.5 -0.5 -0.5) (vec3 0.0 0.0 -1.0) (vec2 0.0 1.0))   ;; 4   right-bottom-front
                          (list (vec3 -0.5 -0.5 -0.5) (vec3 0.0 0.0 -1.0) (vec2 1.0 1.0))  ;; 5   left-bottom-front
                          (list (vec3 -0.5 0.5 -0.5) (vec3 0.0 0.0 -1.0) (vec2 1.0 0.0))   ;; 6   left-top-front
                          (list (vec3 0.5 0.5 -0.5) (vec3 0.0 0.0 -1.0) (vec2 0.0 0.0))    ;; 7   right-top-front
                          (list (vec3 -0.5 -0.5 -0.5) (vec3 -1.0 0.0 0.0) (vec2 0.0 1.0))  ;; 8   left-bottom-front
                          (list (vec3 -0.5 -0.5 0.5) (vec3 -1.0 0.0 0.0) (vec2 1.0 1.0))   ;; 9   left-bottom-back
                          (list (vec3 -0.5 0.5 0.5) (vec3 -1.0 0.0 0.0) (vec2 1.0 0.0))    ;; 10  left-top-back
                          (list (vec3 -0.5 0.5 -0.5) (vec3 -1.0 0.0 0.0) (vec2 0.0 0.0))   ;; 11  left-top-front
                          (list (vec3 0.5 -0.5 0.5) (vec3 1.0 0.0 0.0) (vec2 0.0 1.0))     ;; 12  right-bottom-back
                          (list (vec3 0.5 -0.5 -0.5) (vec3 1.0 0.0 0.0) (vec2 1.0 1.0))    ;; 13  right-bottom-front
                          (list (vec3 0.5 0.5 -0.5) (vec3 1.0 0.0 0.0) (vec2 1.0 0.0))     ;; 14  right-top-front
                          (list (vec3 0.5 0.5 0.5) (vec3 1.0 0.0 0.0) (vec2 0.0 0.0))      ;; 15  right-top-back
                          (list (vec3 -0.5 0.5 0.5) (vec3 0.0 1.0 0.0) (vec2 0.0 1.0))     ;; 16  left-top-back
                          (list (vec3 0.5 0.5 0.5) (vec3 0.0 1.0 0.0) (vec2 1.0 1.0))      ;; 17  right-top-back
                          (list (vec3 0.5 0.5 -0.5) (vec3 0.0 1.0 0.0) (vec2 1.0 0.0))     ;; 18  right-top-front
                          (list (vec3 -0.5 0.5 -0.5) (vec3 0.0 1.0 0.0) (vec2 0.0 0.0))    ;; 19  left-top-front
                          (list (vec3 -0.5 -0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 0.0 1.0))  ;; 20  left-bottom-front
                          (list (vec3 0.5 -0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 1.0))   ;; 21  right-bottom-front
                          (list (vec3 0.5 -0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))    ;; 22  right-bottom-back
                          (list (vec3 -0.5 -0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 0.0 0.0)))) ;; 23  left-bottom-back
      (+cube-index-array+ (list 0 1 2 0 2 3 4 5 6 4 6 7 8 9 10 8 10 11 12 13 14 12 14 15 16 17 18 16 18 19 20
                                21 22 20 22 23)))

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

  (defun side-to-face-indices (side &optional (start-index 0))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; left right above below ahead behind
    ;; 0    1     2     3     4     5
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (declare (fixnum side))
    (mapcar (lambda (x) (+ x start-index))
            (case side
              (0 (list 8 9 10 8 10 11))
              (1 (list 12 13 14 12 14 15))
              (2 (list 16 17 18 16 18 19))
              (3 (list 20 21 22 20 22 23))
              (4 (list 0 1 2 0 2 3))
              (5 (list 4 5 6 4 6 7)))))

  (defun offset-cube-mesh-verts-by-pos (pos &key (spacing 1.0))
    (loop for vert in +cube-vert-array+
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
                             nconcing (side-to-face-indices side vert-start-index)))))))
