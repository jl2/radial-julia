;;;; radial-julia.lisp
;;;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:radial-julia)

(declaim (optimize (speed 3) (safety 0) (compilation-speed 0) (debug 0)))

(declaim (ftype (function 
                 (fixnum fixnum double-float double-float ) double-float) map-val))

(declaim (inline map-val set-pixel-ong black-and-white))
                 
(defun map-val (x width xmin xmax)
  "Map a value from the range 0,width to the range xmin,xmax"
  (declare (type fixnum x width))
  (declare (type double-float xmin xmax))
  (+ xmin (* (- xmax xmin) (/ (coerce x 'double-float) (coerce width 'double-float) 1.0d0))))

(defun set-pixel-png (img x y r g b)
  "Set a pixel in im at location x,y to color (r,g,b)"
  (declare (type fixnum x y r g b))
  (declare (type (simple-array (unsigned-byte 1)) img))
  (setf (aref img x y 0) r)
  (setf (aref img x y 1) g)
  (setf (aref img x y 2) b))

(defun black-and-white  (iters iterations i j width height)
  (declare (ignorable iters iterations i j width height))
  (let ((val (* 255 (mod iters 2))))
    (values val val val)))

(defun draw-radial-julia-line (i png c width height rmin rmax tmin tmax iterations)
  (let ((rp (map-val i height rmin rmax)))
    (dotimes (j width)
      (declare (type fixnum j))
      (let ((iters
             (do* ((tp (map-val j width tmax tmin))
                   (cp (complex (* rp (sin tp)) (* rp (cos tp))) (+ (* cp cp) c))
                   (iter 0 (incf iter)))
                  ((or (>= iter iterations) (> (abs cp) 4.0)) iter)
               (declare (type fixnum iter)))))
        (declare (type fixnum iters))
        (multiple-value-call #'set-pixel-png png i j (black-and-white iters iterations i j width height))))))

(defun make-radial-julia (&key
                            (c #C(0.25 0.25))
                            (file-name)
                            (width 400) (height 400)
                            (rmin 0.0) (rmax 1.25)
                            (tmin 0.0) (tmax (* 2 pi))
                            (iterations 100)
                            (thread-count 8))
  "Generate a Mandelbrot Set fractal and save to the file name given.  The portion of the set drawn is given by xmin,xmax and ymin,ymax."
  (declare (type fixnum width height iterations)
           (type double-float rmin rmax tmin tmax))

  (ensure-directories-exist file-name)
  (let* ((img (png:make-image height width 3 8))
         (wq (wq:create-work-queue (rcurry #'draw-radial-julia-line img c width height rmin rmax tmin tmax iterations) thread-count)))

    (dotimes (i height)
      (declare (type fixnum i))
      (wq:add-job wq i))

    (wq:destroy-work-queue wq)

    (with-open-file (output file-name :element-type '(unsigned-byte 8) :direction :output :if-exists :supersede)
      (png:encode img output))))

(defun make-radial-julia-animation (&key
                               (frames 120)
                               (location-function (lambda (i)
                                                    (let ((tval (* (/ 1.0 120) i)))
                                                      (complex (- tval 1.0)
                                                               (sin tval)))))
                               (output-directory)
                               (width 800) (height 800)
                               (rmin 0.0) (rmax 1.25)
                               (tmin 0.0) (tmax (* 2 pi))
                               (iterations 100)
                               (thread-count 8))
  (let* ((real-dir-name (ensure-directories-exist
                        (if (char=  #\/ (aref output-directory (- (length output-directory) 1)))
                            output-directory
                            (concatenate 'string output-directory "/"))))
         (description-file-name (format nil "~adescription.lisp" real-dir-name)))
    (with-open-file (outf description-file-name :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format outf "(list ~%")
      (dotimes (i frames)
        (let ((output-file-name (format nil "~aframe~5,'0d.png" real-dir-name i))
              (point (funcall location-function i)))
          (format t "Drawing Julia set: ~a~%" point)
          (format outf "~a~%" point)
          (make-radial-julia :file-name output-file-name
                      :width width :height height
                      :c point
                      :rmin rmin
                      :rmax rmax
                      :tmin tmin
                      :tmax tmax
                      :iterations iterations
                      :thread-count thread-count)))
      (format outf ")~%"))))

(defun random-walk-radial-julia-animation (&key
                                      (output-directory "/Users/jeremiahlarocco/images/fractals/julia-animation/")
                                      (frame-count 360)
                                      (start-point (complex (- (random 1.0) 0.75) (random 0.5)))
                                      (change-direction-prob 0.125)
                                      (dt 0.01)
                                      (width 800)
                                      (height 800)
                                      (iterations 80)
                                      (rmin 0.0)
                                      (rmax 1.25)
                                      (tmin 0.0)
                                      (tmax (* 2 pi))
                                      (lower-bound (complex -1.0 -1.0))
                                      (upper-bound (complex 1.0 1.0)))
  (let* (
         (current-location start-point)
         (real-dir 1)
         (imag-dir 1))
    
    (flet ((point-compute (i)
             (declare (ignorable i))
             (incf current-location (complex (* real-dir (random dt)) (* imag-dir (random dt))))
             (format t "~a ~a ~a ~a ~a ~%" current-location upper-bound lower-bound real-dir imag-dir)
             (when (> change-direction-prob (random 1.0))
               (setf real-dir (- real-dir)))

             (when (> change-direction-prob (random 1.0))
               (setf imag-dir (- imag-dir)))

             (when (> (realpart current-location) (realpart upper-bound))
               (format t "Reversing real-dir~%")
               (setf real-dir (- real-dir)))
             (when (< (realpart current-location) (realpart lower-bound))
               (format t "Reversing real-dir~%")
               (setf real-dir (- real-dir)))

             (when (> (imagpart current-location) (imagpart upper-bound))
               (format t "Reversing imag-dir~%")
               (setf imag-dir (- imag-dir)))
             (when (< (imagpart current-location) (imagpart lower-bound))
               (format t "Reversing imag-dir~%")
               (setf imag-dir (- imag-dir)))

             current-location))
      (make-radial-julia-animation :output-directory output-directory
                                    :location-function #'point-compute
                                    :frames frame-count
                                    :rmin rmin :rmax rmax
                                    :tmin tmin :tmax tmax
                                    :iterations iterations
                                    :width width :height height))))
