
;; This is only here to remind me what is needed.
;; (require :cl-gtk2-gtk)
;; (require :lispbuilder-sdl)
;; (require :cl-opengl)
;; (require :cl-glu)
;; (require :cl-gtk2-gtkglext)

;; To deal with cl-gtk2-gtkglext, I had to manually add the path to
;; the asd file to my systems path and manually ask quicklisp to load
;; it. Apparently it doesn't build by default and it is turned off in
;; the quicklisp index. This means that in your ~quicklisp/local-projects
;; directory, make a symlink from gtk-glext to 
;; $HOME/quicklisp/dists/quicklisp/software/cl-gtk2-20120909-git/gtk-glext/
;; and ensure the date is correct for your release of quicklisp.

(in-package #:gtk-opengl-ex1)

(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

(defstruct color (red 1.0) (green 1.0) (blue 1.0) (alpha 1.0))

(defvar *color* (make-color))
(defvar *timeout* nil)

(defun quit-button-clicked (w)
  (declare (ignore w))
  (format t "Quitting~%")
  ;; Remove the timeout before quitting so I don't have a race in
  ;; invalidating the drawing area and then trying to render it.
  (when *timeout*
    (glib:g-source-remove *timeout*)
    (setf *timeout* nil))
  (gtk:gtk-main-quit))

(defun white-button-clicked (w)
  (declare (ignore w))
  (setf (color-red *color*) 1.0
        (color-green *color*) 1.0
        (color-blue *color*) 1.0)
  (format t "White button clicked!~%"))

(defun red-button-clicked (w)
  (declare (ignore w))
  (setf (color-red *color*) 1.0
        (color-green *color*) 0.0
        (color-blue *color*) 0.0)
  (format t "Red button clicked!~%"))

(defun green-button-clicked (w)
  (declare (ignore w))
  (setf (color-red *color*) 0.0
        (color-green *color*) 1.0
        (color-blue *color*) 0.0)
  (format t "Green button clicked!~%"))

(defun blue-button-clicked (w)
  (declare (ignore w))
  (setf (color-red *color*) 0.0
        (color-green *color*) 0.0
        (color-blue *color*) 1.0)
  (format t "Blue button clicked!~%"))

(defun create-opengl-context ()
  (let ((context (gtkglext::gdk-gl-config-new-by-mode '(:rgba :depth :double))))
    context))

;; XXX THIS IS THE BUG
;; Correct lambda-list: (defun da-key-change-event (widget event)
;; Incorrect lambda-list below
(defun da-key-change-event (widget)
  (format t "Got a key change [~A] event: ~A!~%"
	  (gdk:event-key-type event) event)
  (finish-output)
  t)

(defun da-button-press-event (widget event)
  (format t "Got a button-press event: [x=~D, y=~D]!~%"
          (gdk:event-button-x event)
          (gdk:event-button-y event))
  (finish-output)
  t)

(let ((foo 0))
  (defun draw (widget event)
    (declare (ignore widget event))
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho -1 1 -1 1 -1 1)
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (gl:clear-color 0 0 0 0)

    (gl:color (color-red *color*) (color-green *color*) (color-blue *color*))
    (gl:rotate foo 0.0 0.0 1.0)
    (gl:with-primitive :line-loop
      (gl:vertex -.5 -.5 0)
      (gl:vertex -.5 .5 0)
      (gl:vertex .5 .5 0)
      (gl:vertex .5 -.5 0))

    (incf foo 1)
    (gl:flush)))

(defun idle-func (widget)
  ;;(format t "window ~A~%" (gtk:widget-window widget))
  ;;(finish-output)
  (if (gtk:widget-window widget)
      (progn
        (gtk:widget-queue-draw widget)
        t)
      nil))

(defun add-signal-masks (widget &rest masks)
  (when masks
    (dolist (mask masks)
      (pushnew mask (gdk:gdk-window-events (gtk:widget-window widget))))))

(defun doit ()
  (let (top-level)
    (gtk:within-main-loop
      (let ((builder (make-instance 'gtk:builder))
            (opengl-context (create-opengl-context)))

        (gtk:builder-add-from-file builder "./gtk-opengl-ex1.glade")

        (setf top-level (gtk:builder-get-object builder "top_level"))
        (format t "Created toplevel ~A~%" top-level)

        (let ((da (make-instance 'gtkglext:gl-drawing-area
                                 :on-expose #'draw)))

          (gtkglext:gtk-widget-set-gl-capability da opengl-context
                                                 nil t :rgba-type)

          (gtk:box-pack-end (gtk:builder-get-object builder "hbox1") da)

          ;; XXX How do I set the event mask for the drawing area?
          (gobject:connect-signal
           da "realize"
           (lambda (widget)
             (declare (ignore widget))
             (format t "Realize called.~%")
             (add-signal-masks da :button-press-mask :focus-change-mask
                               :key-press-mask)))


          (format t "Connecting drawing area signals~%")
          (gobject:connect-signal da "key-press-event" #'da-key-change-event)
          (gobject:connect-signal da "key-release-event" #'da-key-change-event)
          (gobject:connect-signal da "button-press-event" #'da-button-press-event)

          ;; XXX Set focus for drawing area?
          (pushnew :can-focus (gtk:widget-flags da))
          (gtk:widget-grab-focus da)

          ;; 60 fps
          (setf *timeout*
                (gtk:gtk-main-add-timeout
                 16
                 #'(lambda ()
                     (idle-func da))))

          (gtk:builder-connect-signals-simple
           builder `(("quit_button_clicked" ,#'quit-button-clicked)
                     ("white_button_clicked" ,#'white-button-clicked)
                     ("red_button_clicked" ,#'red-button-clicked)
                     ("green_button_clicked" ,#'green-button-clicked)
                     ("blue_button_clicked" ,#'blue-button-clicked)))

          (gtk:widget-show top-level :all t))))

    (gtk:join-gtk-main)

    (gtk:within-main-loop
      (gtk:object-destroy top-level))))

