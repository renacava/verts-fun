(defclass material ()
  ((strength
    :initform 50.0
    :initarg :strength
    :accessor strength)
   (weight
    :initform 50.0
    :initarg :weight
    :accessor weight)
   (texture
    :initform "default"
    :initarg :texture
    :accessor texture)
   (tags
    :initarg :tags
    :initform nil
    :accessor tags)))

(defgeneric material-combine (material1 material2)
  (:documentation "Combines the two given material instances, returns result."))

(defmethod material-combine ((mat1 material) (mat2 material))
  (make-instance 'material
                 :tags (append (enlist (tags mat1)) 
                               (enlist (tags mat2)))
                 :texture (texture mat1)
                 :weight (average (weight mat1)
                                  (weight mat2))
                 :strength (average (strength mat1)
                                    (strength mat2))))
