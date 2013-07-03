;;;; tic-tac-toe.asd

(asdf:defsystem #:tic-tac-toe
  :serial t
  :description "tic-tac-toe game"
  :author "Mick Beaver <m.charles.beaver@gmail.com>"
  :license "MIT"
  :depends-on (#:cl-glut
               #:cl-opengl)
  :components ((:file "package")
               (:file "tic-tac-toe")))

