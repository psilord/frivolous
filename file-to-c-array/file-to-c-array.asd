;;;; file-to-c-array.asd

(asdf:defsystem #:file-to-c-array
  :serial t
  :description "Convert a file to a c-style array of unsigned characters"
  :author "Mick Beaver <m.charles.beaver@gmail.com>"
  :license "MIT"
  :depends-on (#:getopt)
  :components ((:file "package")
               (:file "file-to-c-array")))

