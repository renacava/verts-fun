(in-package :verts-fun)

(defun-g gui-vert-shader ((vert g-pt)
                          &uniform
                          (perspective :mat4)
                          (offset :vec2)
                          (scale :float))
  (let* ((pos (pos vert))
         (offset (/ offset scale))
         (pos (vec3 (+ (aref pos 0)
                       (aref offset 0))
                    (+ (aref pos 1)
                       (aref offset 1))
                    (aref pos 2)))
         (pos (* pos scale))
         (colour (vec3 1.0 1.0 1.0)))
    (values (* perspective (v! (aref pos 0) (aref pos 1) -1.0 1.0))
            (:smooth colour)
            (tex vert))))

(defun-g gui-frag-shader ((colour :vec3)
                          (uv :vec2)
                          &uniform
                          (2d-sampler :sampler-2d))
  (let* ((result (texture 2d-sampler uv))
         (result (vec4 0.0;;(aref result 0)
                       1.0;;(aref result 1)
                       0.0;;(aref result 2)
                       (aref result 3))))
    result))

(defpipeline-g gui-pipeline ()
  :vertex (gui-vert-shader g-pt)
  :fragment (gui-frag-shader :vec3 :vec2))

(defun rect-mesh (&key (width 1.0) (height 1.0) (u-start 0.0) (v-start 0.0) (u-end 1.0) (v-end 1.0))
  "Returns two values: a gpu-array of uv-mapped rect-verts, and a gpu-array of vert-indices for the rect. Please remember to free it when you're done."
  (let* ((width (half width))
         (height (half height)))
    (values
     (make-gpu-array (list (list (v! (- width) (- height) 0.0) (v! u-start v-end))
                           (list (v! (+ width) (- height) 0.0) (v! u-end v-end))
                           (list (v! (+ width) (+ height) 0.0) (v! u-end v-start))
                           (list (v! (- width) (+ height) 0.0) (v! u-start v-start)))
                     :element-type 'g-pt)
     (make-gpu-array (list 0 1 2 3 0 2)
                     :element-type :uint))))


