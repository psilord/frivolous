(defpackage #:pathogenesis-arcanum-asd
  (:use :cl :asdf))

(in-package #:pathogenesis-arcanum-asd)

(defsystem #:pathogenesis-arcanum
  :description "Pathogenesis Arcanum"
  :version "0.0"
  :author 
  "Peter Keller <psilord@cs.wisc.edu>, Mick Beaver <m.charles.beaver@gmail.com>"
  :license "Apache License, Version 2.0"

  :depends-on (#:alexandria #:cl-opengl #:lispbuilder-sdl)
  :components ((:file "package")
               (:file "pathogenesis-arcanum"
                      :depends-on ("package"))))
