(require :cl-gtk2-gtk)
(require :lispbuilder-sdl)
(require :cl-opengl)
(require :cl-glu)

(defun clicked-callback (w)
  (declare (ignore w))
  (format t "Quitting~%")
  (gtk:gtk-main-quit))

;; To deal with cl-gtk2-gtkglext, I had to manually add the path to
;; the asd file to my systems path and manually ask quicklisp to load
;; it. Apparently it doesn't build by default and it is turned off in
;; the quicklisp index.

(defun doit ()
  (let (top-level)
    (gtk:within-main-loop
     (let ((builder (make-instance 'gtk:builder)))
       (gtk:builder-add-from-file builder "./hello.xml")
       (gtk:builder-connect-signals-simple
        ;;builder `(("quit_button_clicked" ,#'clicked-callback)))
        builder `(("quit_button_clicked" clicked-callback)))

       (setf top-level (gtk:builder-get-object builder "top_level"))
       (format t "Created toplevel ~A~%" top-level)

       (gtk:widget-show top-level :all t)))

    (gtk:join-gtk-main)

    (gtk:within-main-loop
     (gtk:object-destroy top-level))))
