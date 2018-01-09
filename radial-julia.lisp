;;;; radial-julia.lisp
;;;;
;;;; Copyright (c) 2018 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:radial-julia)

(declaim (optimize (speed 3) (safety 1) (compilation-speed 0) (debug 1)))

(declaim (ftype (function 
                 (fixnum fixnum double-float double-float ) double-float) j-map-val))

(declaim (ftype (function
                 (fixnum (simple-array (unsigned-byte 8)) (complex double-float) fixnum fixnum double-float double-float double-float double-float fixnum))
                draw-radial-julia-line))
         
(declaim (inline j-map-val set-pixel-ong black-and-white draw-radial-julia-line smooth-colors))
                 
(defun j-map-val (x width xmin xmax)
  "Map a value from the range 0,width to the range xmin,xmax"
  (declare (type fixnum x width))
  (declare (type double-float xmin xmax))
  (the double-float (+ xmin (* (- xmax xmin) (/ (coerce x 'double-float) (coerce width 'double-float) 1.0d0)))))

(defun set-pixel-png (img x y r g b)
  "Set a pixel in im at location x,y to color (r,g,b)"
  (declare (type fixnum x y r g b))
  (declare (type (simple-array (unsigned-byte 8)) img))
  (setf (aref img x y 0) r)
  (setf (aref img x y 1) g)
  (setf (aref img x y 2) b))

(defun black-and-white  (iters iterations i j width height)
  (declare (ignorable iters iterations i j width height))
  (declare (type fixnum iters iterations i j width height))
  (let ((val (* 255 (mod iters 2))))
    (values val val val)))

(defun smooth-colors (iters iterations i j width height)
  (declare (ignorable iters iterations i j width height))
  (declare (type fixnum iters iterations i j width height))
  (let ((tval (/ (coerce iters 'double-float) (coerce iterations 'double-float) 1.0)))
    (values (truncate (* 255 (- 1 tval)))
            (truncate (+ (* 127 (sin (* 8 pi tval))) 127))
            (truncate (+ (* 127 (cos (* pi tval))) 127)))))

(defun new-colors (iters iterations i j width height)
  (declare (ignorable iters iterations i j width height))
  (declare (type fixnum iters iterations i j width height))
  (let ((rem (mod iters 7))
        (tval (/ (coerce iters 'double-float) (coerce iterations 'double-float) 1.0)))
    (if (= iters iterations)
        (values 0 0 0)
        (values (truncate (* 36 rem))
                (truncate (* 13 rem))
                (truncate (+ (* 127 (cos (* 2 tval))) 127))))))

(defun draw-radial-julia-line (i png c width height rmin rmax tmin tmax iterations)
  (declare (type fixnum i width height iterations)
           (type (complex double-float) c)
           (type double-float rmin rmax tmin tmax)
           (type (simple-array (unsigned-byte 8)) png))
  (let ((rp (j-map-val i height rmin rmax)))
    (dotimes (j width)
      
      (declare (type fixnum j)
               (type double-float rp))
      (let ((iters
             (do* ((tp (j-map-val j width tmax tmin))
                   (cp (complex (* rp (sin tp)) (* rp (cos tp))) (+ (* cp cp) c))
                   (iter 0 (incf iter)))
                  ((or (>= iter iterations) (> (abs cp) 4.0)) iter)
               (declare (type fixnum iter)
                        (type (complex double-float) cp)
                        (type fixnum iter)
                        (type double-float tp))
               )))
        (declare (type fixnum iters))
        (multiple-value-call #'set-pixel-png png i j (new-colors iters iterations i j width height))))))

(defun make-radial-julia (&key
                            (c #C(0.25 0.25))
                            (file-name)
                            (width 400) (height 400)
                            (rmin 0.0) (rmax 1.25)
                            (tmin 0.0) (tmax (* 2 pi))
                            (iterations 100)
                            (thread-count 8))
  "Generate a Mandelbrot Set fractal and save to the file name given.  The portion of the set drawn is given by xmin,xmax and ymin,ymax."
  (declare (type fixnum width height iterations thread-count)
           (type string file-name)
           (type (complex double-float) c)
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
                                             (thread-count 4)
                                             (lower-bound (complex -1.0 -1.0))
                                             (upper-bound (complex 1.0 1.0)))
  (declare (type fixnum width height frame-count iterations thread-count)
           (type simple-string output-directory)
           (type (complex double-float) start-point lower-bound upper-bound)
           (type double-float rmin rmax tmin tmax change-direction-prob dt))
  (let* (
         (current-location start-point)
         (real-dir 1.0)
         (imag-dir 1.0)
         (real-dir-name (ensure-directories-exist
                        (if (char=  #\/ (aref output-directory (- (length output-directory) 1)))
                            output-directory
                            (concatenate 'string output-directory "/"))))
         (description-file-name (format nil "~adescription.lisp" real-dir-name)))
    (declare (type double-float real-dir imag-dir)
             (type (complex double-float) current-location)
             (type simple-string real-dir-name description-file-name))
    (with-open-file (outf description-file-name :direction :output :if-exists :supersede :if-does-not-exist :create)
      (declare (type stream outf))
      (format outf "(list ~%")
      (dotimes (i frame-count)
        (declare (type fixnum i))
        (let ((output-file-name (format nil "~aframe~5,'0d.png" real-dir-name i)))
          (format t "Drawing Julia set: ~a~%" current-location)
          (format outf "~a~%" current-location)
          (make-radial-julia :file-name output-file-name
                             :width width :height height
                             :c current-location
                             :rmin rmin
                             :rmax rmax
                             :tmin tmin
                             :tmax tmax
                             :iterations iterations
                             :thread-count thread-count)

          (incf current-location (complex
                                  (* real-dir (random dt))
                                  (* imag-dir (random dt))))
          ;;(format t "~a ~a ~a ~a ~a ~%" current-location upper-bound lower-bound real-dir imag-dir)
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
          ))
      (format outf ")~%"))))

(defun rj-fft (&key
                 (mp3-file-name)
                 (output-directory "/Users/jeremiahlarocco/images/fractals/julia-animation/")
                 (start-point #C(0.31520945078498014 0.374594080698711))
                 (width 800)
                 (height 800)
                 (iterations 80)
                 (rmin 0.0)
                 (rmax 1.25)
                 (tmin 0.0)
                 (tmax (* 2 pi))
                 (fps 30)
                 (thread-count 4)
                 (fft-window-size 64)
                 (change-direction-prob 0.005)
                 (real-min -0.5)
                 (real-max 0.5)
                 (imag-min -0.5)
                 (imag-max 0.5))
  
  (declare (type fixnum width height frame-count iterations thread-count)
           (type simple-string output-directory mp3-file-name)
           (type (complex double-float) start-point)
           (type double-float real-min real-max imag-min imag-max)
           (type double-float rmin rmax tmin tmax)
           )
  (let* (
         (real-dir-name (ensure-directories-exist
                        (if (char=  #\/ (aref output-directory (- (length output-directory) 1)))
                            output-directory
                            (concatenate 'string output-directory "/"))))
         (description-file-name (format nil "~adescription.lisp" real-dir-name))

         (the-mp3 (read-mp3-file mp3-file-name))
         (real-dir 1.0)
         (imag-dir 1.0)
         (current-location start-point)
         (song-duration (mp3-file-duration-in-seconds the-mp3))
         (total-frames (ceiling (* song-duration fps)))
         
         )
    (declare 
             (type (complex double-float) current-location)
             (type simple-string real-dir-name description-file-name))

    (format t "Creating animation with ~a frames...~%" total-frames)

    (with-open-file (outf description-file-name :direction :output :if-exists :supersede :if-does-not-exist :create)
      (declare (type stream outf))
      (format outf "(list ~%")
      (dotimes (frame total-frames)
        (declare (type fixnum frame))
        (let* ((output-file-name (format nil "~aframe~8,'0d.png" real-dir-name frame))
               (win-center (ceiling (max 0 (- (* 44100 (interpolate 0.0 song-duration frame total-frames))
                                              (round (/ fft-window-size 2))))))
               
               (left-fft-data (bordeaux-fft:windowed-fft (mp3-file-left-channel the-mp3) win-center fft-window-size))
               (right-fft-data (bordeaux-fft:windowed-fft (mp3-file-right-channel the-mp3) win-center fft-window-size)))

          (incf current-location (/ (+ (aref left-fft-data 1) (aref right-fft-data 1)) fft-window-size))
          ;; (incf current-location (complex (/ (* real-dir (abs (aref left-fft-data 2))) fft-window-size)
          ;;                                 (/ (* imag-dir (abs (aref right-fft-data 2))) fft-window-size)))

          (format t "Drawing Julia set: ~a~%" current-location)
          (when (> change-direction-prob (random 1.0))
            (setf real-dir (- real-dir)))

          (when (> change-direction-prob (random 1.0))
            (setf imag-dir (- imag-dir)))

          (when (> (realpart current-location) real-max)
            (format t "Reversing real-dir~%")
            (setf current-location (complex  real-max (imagpart current-location)))
            (setf real-dir (- real-dir)))
          (when (< (realpart current-location) real-min)
            (format t "Reversing real-dir~%")
            (setf current-location (complex real-min (imagpart current-location)))
            (setf real-dir (- real-dir)))

          (when (> (imagpart current-location) imag-max)
            (format t "Reversing imag-dir~%")
            (setf current-location (complex (realpart current-location) imag-max))
            (setf imag-dir (- imag-dir)))
          (when (< (imagpart current-location) imag-min)
            (format t "Reversing imag-dir~%")
            (setf current-location (complex (realpart current-location) imag-min))
            (setf imag-dir (- imag-dir)))
          (format t "Drawing Julia set: ~a~%" current-location)
          (format outf "~a~%" current-location)
          (make-radial-julia :file-name output-file-name
                             :width width :height height
                             :c current-location
                             :rmin rmin
                             :rmax rmax
                             :tmin tmin
                             :tmax tmax
                             :iterations iterations
                             :thread-count thread-count)))
      (format outf ")~%"))))



(defun make-movie (directory mp3-name final-name tmp-name &optional (remove-tmp t) (bit-rate (* 4 2014)))
  "Run ffmpeg to create a movie with audio."
  (if (probe-file tmp-name)
      (delete-file tmp-name))

  (let ((movie-command
         (format nil 
                 "ffmpeg -r 30 -i \"~aframe%08d.png\" -b ~a -q 4 \"~a\""
                 directory bit-rate tmp-name))
        (audio-command
         (format nil
                 "ffmpeg -i \"~a\" -i \"~a\" -codec copy -shortest \"~a\""
                 tmp-name mp3-name final-name)))
    
    (format t "~a~%" movie-command)
    (uiop:run-program movie-command)
    (if (probe-file final-name)
        (delete-file final-name))

    (format t "~a~%" audio-command)
    (uiop:run-program audio-command)
    (if remove-tmp
        (delete-file tmp-name))))
