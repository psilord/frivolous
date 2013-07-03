(defpackage #:opengl-template-asd
  (:use :cl :asdf))

(in-package #:opengl-template-asd)

(defsystem #:opengl-template
  :description "Template of opengl program"
  :version "0.0"
  :author "Peter Keller <psilord@cs.wisc.edu>"
  :license "Apache License, Version 2.0"

  :depends-on (#:alexandria #:cl-opengl #:lispbuilder-sdl)
  :components ((:file "package")
               (:file "opengl-template"
                      :depends-on ("package"))))
