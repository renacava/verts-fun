(let ((+cube-vert-array+ (list
                          (list (vec3 -0.5 -0.5 0.5) (vec3 0.0 0.0 1.0) (vec2 0.0 1.0))    ;; left-bottom-back
                          (list (vec3 0.5 -0.5 0.5) (vec3 0.0 0.0 1.0) (vec2 1.0 1.0))     ;; right-bottom-back
                          (list (vec3 0.5 0.5 0.5) (vec3 0.0 0.0 1.0) (vec2 1.0 0.0))      ;; right-top-back
                          (list (vec3 -0.5 0.5 0.5) (vec3 0.0 0.0 1.0) (vec2 0.0 0.0))     ;; left-top-back
                          (list (vec3 0.5 -0.5 -0.5) (vec3 0.0 0.0 -1.0) (vec2 0.0 1.0))   ;; right-bottom-front
                          (list (vec3 -0.5 -0.5 -0.5) (vec3 0.0 0.0 -1.0) (vec2 1.0 1.0))  ;; left-bottom-front
                          (list (vec3 -0.5 0.5 -0.5) (vec3 0.0 0.0 -1.0) (vec2 1.0 0.0))   ;; left-top-front
                          (list (vec3 0.5 0.5 -0.5) (vec3 0.0 0.0 -1.0) (vec2 0.0 0.0))    ;; right-top-front
                          (list (vec3 -0.5 -0.5 -0.5) (vec3 -1.0 0.0 0.0) (vec2 0.0 1.0))  ;; left-bottom-front
                          (list (vec3 -0.5 -0.5 0.5) (vec3 -1.0 0.0 0.0) (vec2 1.0 1.0))   ;; left-bottom-back
                          (list (vec3 -0.5 0.5 0.5) (vec3 -1.0 0.0 0.0) (vec2 1.0 0.0))    ;; left-top-back
                          (list (vec3 -0.5 0.5 -0.5) (vec3 -1.0 0.0 0.0) (vec2 0.0 0.0))   ;; left-top-front
                          (list (vec3 0.5 -0.5 0.5) (vec3 1.0 0.0 0.0) (vec2 0.0 1.0))     ;; right-bottom-back
                          (list (vec3 0.5 -0.5 -0.5) (vec3 1.0 0.0 0.0) (vec2 1.0 1.0))    ;; right-bottom-front
                          (list (vec3 0.5 0.5 -0.5) (vec3 1.0 0.0 0.0) (vec2 1.0 0.0))     ;; right-top-front
                          (list (vec3 0.5 0.5 0.5) (vec3 1.0 0.0 0.0) (vec2 0.0 0.0))      ;; right-top-back
                          (list (vec3 -0.5 0.5 0.5) (vec3 0.0 1.0 0.0) (vec2 0.0 1.0))     ;; left-top-back
                          (list (vec3 0.5 0.5 0.5) (vec3 0.0 1.0 0.0) (vec2 1.0 1.0))      ;; right-top-back
                          (list (vec3 0.5 0.5 -0.5) (vec3 0.0 1.0 0.0) (vec2 1.0 0.0))     ;; right-top-front
                          (list (vec3 -0.5 0.5 -0.5) (vec3 0.0 1.0 0.0) (vec2 0.0 0.0))    ;; left-top-front
                          (list (vec3 -0.5 -0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 0.0 1.0))  ;; left-bottom-front
                          (list (vec3 0.5 -0.5 -0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 1.0))   ;; right-bottom-front
                          (list (vec3 0.5 -0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 1.0 0.0))    ;; right-bottom-back
                          (list (vec3 -0.5 -0.5 0.5) (vec3 0.0 -1.0 0.0) (vec2 0.0 0.0)))) ;; left-bottom-back
      (+cube-index-array+ (list 0 1 2 0 2 3 4 5 6 4 6 7 8 9 10 8 10 11 12 13 14 12 14 15 16 17 18 16 18 19 20
                                21 22 20 22 23)))

  )

