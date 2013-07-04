(defpackage #:gtk-opengl-ex1
  ;;(:use #:cl #:cl-opengl #:cl-glu #:lispbuilder-sdl #:cl-gtk2-gtk)
  (:use #:cl)
  (:export #:doit))

(in-package #:gtk-opengl-ex1)

;; (declaim (optimize (safety 0) (space 0) (speed 3) (debug 0)))
(declaim (optimize (safety 3) (space 0) (speed 0) (debug 3)))

