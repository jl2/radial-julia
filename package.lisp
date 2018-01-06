;;;; package.lisp
;;;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(defpackage #:radial-julia
  (:nicknames #:rj)
  (:use #:cl #:alexandria)
  (:export
   #:make-radial-julia
   #:make-radial-julia-animation
   #:random-walk-radial-julia-animation
   ))
