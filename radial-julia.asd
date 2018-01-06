;;;; radial-julia.asd
;;;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:radial-julia
  :description "Describe radial-julia here"
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC"
  :depends-on (#:png
               #:alexandria
               #:work-queue)
  :serial t
  :components ((:file "package")
               (:file "radial-julia")))

