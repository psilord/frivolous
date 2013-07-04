;; Copyright 2010 Peter K. Keller (psilord@cs.wisc.edu)
;;
;; Licensed under the Apache License, Version 2.0 (the "License"); you
;; may not use this file except in compliance with the License. You may
;; obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
;; or implied. See the License for the specific language governing
;; permissions and limitations under the License.

(defpackage #:gtk-opengl-ex1-asd
  (:use :cl :asdf))

(in-package #:gtk-opengl-ex1-asd)

(defsystem #:gtk-opengl-ex1
  :description "Testing GTK2 and OpenGL with Common Lisp"
  :version "0.1"
  :author "Peter Keller <psilord@cs.wisc.edu>"
  :license "Apache License, Version 2.0"

  :depends-on (#:cl-opengl #:cl-glu #:lispbuilder-sdl #:cl-gtk2-gtk
			   #:cl-gtk2-gtkglext)
  :components ((:file "package")
               (:file "gtk-opengl-ex1"
                      :depends-on ("package"))))
