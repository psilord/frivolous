;;;; tic-tac-toe.lisp

(in-package #:tic-tac-toe)

(defclass ttt-window (glut:window)
  ()
  (:default-initargs :width 600 :height 600 :title "tic-tac-toe.lisp"
		     :mode '(:double :rgb)))

(defmethod glut:display-window :before ((window ttt-window))
  ;; Good spot for initial OpenGL setup
  (gl:enable :line-smooth)
  (gl:line-width 10)
  (gl:clear-color 0 0 0 0)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 100 0 100 -1 1))

(defmethod glut:display ((window ttt-window))
  (gl:clear :color-buffer)
  (gl:color 1 1 1)
  (gl:with-primitive :lines
    (gl:vertex 33 0)
    (gl:vertex 33 100))
  (gl:with-primitive :lines
    (gl:vertex 66 0)
    (gl:vertex 66 100))
  (gl:with-primitive :lines
    (gl:vertex 0 33)
    (gl:vertex 100 33))
  (gl:with-primitive :lines
    (gl:vertex 0 66)
    (gl:vertex 100 66))
  (glut:swap-buffers))

(defmethod glut:idle ((window ttt-window))
  (glut:post-redisplay))

(defmethod glut:reshape ((window ttt-window) width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 100 0 100 -1 1)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defmethod glut:keyboard ((window ttt-window) key x y)
  (declare (ignore x y))
  (when (eql key #\Esc)
    (glut:destroy-current-window)))

(defmethod glut:mouse ((window ttt-window) button state x y)
  (case button
    (:left-button
     (when (eq state :up)
       (format *query-io* "Left click at pixel ~a ~a~%" x y)
       (force-output *query-io*)))))

(defun main ()
  (glut:display-window (make-instance 'ttt-window)))
   
