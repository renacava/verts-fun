(in-package #:verts-fun)

(defmacro %case-event ((event) &body event-handlers)
  (assert (symbolp event))
  `(case (sdl2::get-event-type ,event)
     ,@(loop :for (type params . forms) :in event-handlers
             :append (let ((type (if (listp type)
                                     type
                                     (list type))))
                       (loop :for typ :in type :collect
                             (sdl2::expand-handler event typ params forms)))
             :into results
             :finally (return (remove nil results)))))

(defun on-game-input (event &optional tpref)
  "This runs when the game receives an input event."
  (%case-event (event)
    
    (:mousewheel
     (:timestamp ts :which id :x x :y y)
     (let ((mouse (mouse id)))
       (print (format nil "MOUSEWHEEL EVENT: X: ~A :Y ~A" x y))))

    ((:mousebuttondown :mousebuttonup)
     (:timestamp ts :which id :button b :state s :x x :y y)
     (let ((mouse (mouse id)))
       
       ))

    (:mousemotion
     (:timestamp ts :which id :x x :y y :xrel xrel :yrel yrel)
     (let ((mouse (mouse id)))
       
       ))

    ((:keydown :keyup)
     (:timestamp ts :state s :keysym keysym)
     (let ((kbd (keyboard 0))
           (s (if (= s 0) 'released 'pressed))
           (key (keysym-to-property keysym)))
       (print (format nil "Keyboard event:~%~5tts: ~a~%~5tstate: ~a~%~5tkey: ~a"
                      ts s key))
       (when (and (eq key :escape)
                  (eq s 'released))
         (stop))))))

(defun on-game-input-outer (event &optional tpref)
  (funcall (lambda () (on-game-input event tpref))))

(cepl:register-event-listener #'verts-fun::on-game-input-outer)
