(in-package :verts-fun)
;; this contains all the keys for skitter.sdl2.
;; use (key.id :key) to grab the ID of the key.
;;skitter.sdl2::*key-button-names*

(def-suite keyboard
  :description "Tests for the keyboard"
  :in modulus)

(in-suite keyboard)

(defclass key-state (moduclass)
  ((key-name
    :initarg :key-name)
   (key-down
    :initform nil)
   (just-pressed
    :initform nil)
   (just-released
    :initform nil)))

(defgeneric key-name (key-state)
  (:documentation "Returns a :key correlating to the name of the given key-state object."))

(defmethod key-name ((key-state key-state))
  (slot-value key-state 'key-name))

(defgeneric key-down (key-state)
  (:documentation "Returns T if the given key-state is marked as being down."))

(defmethod key-down ((key-state key-state))
  (slot-value key-state 'key-down))

(defgeneric key-press (key-state)
  (:documentation "Sets the given key-state object to be just-pressed and down."))

(defmethod key-press ((key-state key-state))
  (setf (slot-value key-state 'key-down) t
        (slot-value key-state 'just-pressed) t))

(defgeneric key-just-pressed (key-state)
  (:documentation "Returns T if the given key-state is just-pressed."))

(defmethod key-just-pressed ((key-state key-state))
  (slot-value key-state 'just-pressed))

(defgeneric key-just-released (key-state)
  (:documentation "Returns T if the given key-state is just-released."))

(defmethod key-just-released ((key-state key-state))
  (slot-value key-state 'just-released))

(defgeneric key-step (key-state)
  (:documentation "Steps the given key-state object. just-pressed and just-released should be nil after this."))

(defmethod key-step ((key-state key-state))
  (setf (slot-value key-state 'just-pressed) nil
        (slot-value key-state 'just-released) nil))

(defgeneric key-release (key-state)
  (:documentation "Sets the given key-state to be released."))

(defmethod key-release ((key-state key-state))
  (setf (slot-value key-state 'key-down) nil
        (slot-value key-state 'just-released) t))

(test key-state
  (let ((my-key-state (make-instance 'key-state :key-name :home)))
    (is (equal (key-name my-key-state)
               :home))
    (is (not (key-down my-key-state)))
    
    (key-press my-key-state)
    
    (is (key-down my-key-state))
    (is (key-just-pressed my-key-state))
    (is (not (key-just-released my-key-state)))

    (key-step my-key-state)

    (is (key-down my-key-state))
    (is (not (key-just-pressed my-key-state)))
    (is (not (key-just-released my-key-state)))

    (key-release my-key-state)

    (is (not (key-down my-key-state)))
    (is (not (key-just-pressed my-key-state)))
    (is (key-just-released my-key-state))

    (key-step my-key-state)

    (is (not (key-down my-key-state)))
    (is (not (key-just-pressed my-key-state)))
    (is (not (key-just-released my-key-state)))

    (setf my-key-state nil)))

;; (let ((keyboard-table (make-hash-table)))
;;   )
