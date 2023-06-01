(in-package :verts-fun)

(defun block-at-pos? (pos)
  "Returns T if there's a block at the given pos."
  (and (< (aref pos 1) (+ 6 (* 2 (sin (* 0.5 (aref pos 0))))))
       t
       (oddp (truncate (aref pos 2)))
       (oddp (truncate (aref pos 1)))
       (oddp (truncate (aref pos 0)))
       ))

(defun block-at-pos? (pos)
  "Returns T if there's a block at the given pos."
  (and (< (aref pos 1) (+ 6 (* 2 (sin (* 0.5 (aref pos 0))))))
       t
       (oddp (truncate (aref pos 2)))
       (oddp (truncate (aref pos 1)))
       ;;(oddp (truncate (aref pos 0)))
       ))

(defun block-at-pos? (pos)
  "Returns T if there's a block at the given pos."
  (and (< (aref pos 1) (+ 6 (* 2 (sin (* 0.5 (aref pos 0))))))
       t
       (oddp (truncate (aref pos 2)))
       ;;(oddp (truncate (aref pos 1)))
       ;;(oddp (truncate (aref pos 0)))
       ))

(defun block-at-pos? (pos)
  "Returns T if there's a block at the given pos."
  (and (< (aref pos 1) (+ 6 (* 2 (sin (* 0.5 (aref pos 0))))))
       t
       ;;(oddp (truncate (aref pos 2)))
       ;;(oddp (truncate (aref pos 1)))
       ;;(oddp (truncate (aref pos 0)))
       ))

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
