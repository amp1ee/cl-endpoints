(load #P"/home/amplee/LISP/quicklisp/setup.lisp")
(ql:quickload :ltk)

(defpackage :simple-pong
  (:use :cl :ltk))

(in-package :simple-pong)

(defparameter *width* 800)
(defparameter *height* 500)

(defparameter *paddle-width* 15)
(defparameter *paddle-height* 90)
(defparameter *ball-size* 16)

(defparameter *left-paddle-x* 30)
(defparameter *right-paddle-x* (- *width* 45))

(defparameter *paddle-speed* 7)
(defparameter *ball-speed-x* 5)
(defparameter *ball-speed-y* 4)

(defparameter *left-score* 0)
(defparameter *right-score* 0)

(defparameter *left-paddle-y* (/ (- *height* *paddle-height*) 2))
(defparameter *right-paddle-y* (/ (- *height* *paddle-height*) 2))

(defparameter *ball-x* (/ (- *width* *ball-size*) 2))
(defparameter *ball-y* (/ (- *height* *ball-size*) 2))
(defparameter *dx* *ball-speed-x*)
(defparameter *dy* *ball-speed-y*)

(defparameter *w-pressed* nil)
(defparameter *s-pressed* nil)
(defparameter *up-pressed* nil)
(defparameter *down-pressed* nil)

(defvar *canvas* nil)
(defvar *left-paddle* nil)
(defvar *right-paddle* nil)
(defvar *ball* nil)
(defvar *score-text* nil)
(defvar *center-line* nil)

(defun clamp (value min max)
  (max min (min max value)))

(defun reset-ball ()
  (setf *ball-x* (/ (- *width* *ball-size*) 2)
        *ball-y* (/ (- *height* *ball-size*) 2)
        *dx* (if (= (random 2) 0)
                 *ball-speed-x*
                 (- *ball-speed-x*))
        *dy* (- (random 9) 4))
  (when (= *dy* 0)
    (setf *dy* *ball-speed-y*)))

(defun update-score-text ()
  (itemconfigure *canvas* *score-text* "text"
                 (format nil "~A   :   ~A" *left-score* *right-score*)))

(defun move-paddles ()
  ;; Left paddle: W / S
  (when *w-pressed*
    (decf *left-paddle-y* *paddle-speed*))
  (when *s-pressed*
    (incf *left-paddle-y* *paddle-speed*))

  ;; Right paddle: Up / Down
  (when *up-pressed*
    (decf *right-paddle-y* *paddle-speed*))
  (when *down-pressed*
    (incf *right-paddle-y* *paddle-speed*))

  (setf *left-paddle-y*
        (clamp *left-paddle-y* 0 (- *height* *paddle-height*)))
  (setf *right-paddle-y*
        (clamp *right-paddle-y* 0 (- *height* *paddle-height*))))

(defun ball-hits-left-paddle-p ()
  (and (<= *left-paddle-x* *ball-x* (+ *left-paddle-x* *paddle-width*))
       (<= *left-paddle-y*
           (+ *ball-y* (/ *ball-size* 2))
           (+ *left-paddle-y* *paddle-height*))))

(defun ball-hits-right-paddle-p ()
  (and (<= *right-paddle-x*
           (+ *ball-x* *ball-size*)
           (+ *right-paddle-x* *paddle-width*))
       (<= *right-paddle-y*
           (+ *ball-y* (/ *ball-size* 2))
           (+ *right-paddle-y* *paddle-height*))))

(defun bounce-from-paddle (paddle-y)
  (setf *dx* (- *dx*))
  (let* ((paddle-center (+ paddle-y (/ *paddle-height* 2.0)))
         (ball-center (+ *ball-y* (/ *ball-size* 2.0)))
         (offset (/ (- ball-center paddle-center)
                    (/ *paddle-height* 2.0))))
    (setf *dy* (round (* offset 7)))
    (when (= *dy* 0)
      (setf *dy* (if (>= offset 0) 1 -1)))))

(defun move-ball ()
  (incf *ball-x* *dx*)
  (incf *ball-y* *dy*)

  ;; top/bottom bounce
  (when (<= *ball-y* 0)
    (setf *ball-y* 0
          *dy* (- *dy*)))

  (when (>= *ball-y* (- *height* *ball-size*))
    (setf *ball-y* (- *height* *ball-size*)
          *dy* (- *dy*)))

  ;; left paddle
  (when (and (< *dx* 0)
             (ball-hits-left-paddle-p))
    (setf *ball-x* (+ *left-paddle-x* *paddle-width*))
    (bounce-from-paddle *left-paddle-y*))

  ;; right paddle
  (when (and (> *dx* 0)
             (ball-hits-right-paddle-p))
    (setf *ball-x* (- *right-paddle-x* *ball-size*))
    (bounce-from-paddle *right-paddle-y*))

  ;; score
  (when (< *ball-x* 0)
    (incf *right-score*)
    (update-score-text)
    (reset-ball))

  (when (> *ball-x* *width*)
    (incf *left-score*)
    (update-score-text)
    (reset-ball)))

(defun redraw ()
  (set-coords *canvas* *left-paddle*
              (list *left-paddle-x* *left-paddle-y*
                    (+ *left-paddle-x* *paddle-width*)
                    (+ *left-paddle-y* *paddle-height*)))

  (set-coords *canvas* *right-paddle*
              (list *right-paddle-x* *right-paddle-y*
                    (+ *right-paddle-x* *paddle-width*)
                    (+ *right-paddle-y* *paddle-height*)))

  (set-coords *canvas* *ball*
              (list *ball-x* *ball-y*
                    (+ *ball-x* *ball-size*)
                    (+ *ball-y* *ball-size*))))

(defun game-loop ()
  (move-paddles)
  (move-ball)
  (redraw)
  (after 16 #'game-loop))

(defun start-game ()
  (with-ltk ()
    (let ((root *tk*))
      (wm-title root "Simple Pong (LTK)")

      (setf *canvas*
            (make-instance 'canvas
                           :master root
                           :width *width*
                           :height *height*
                           :background "black"))
      (pack *canvas*)

      ;; dashed center line
      (setf *center-line*
            (create-line *canvas*
                         (list (/ *width* 2) 0
                               (/ *width* 2) *height*)))
      (itemconfigure *canvas* *center-line* "fill" "white")
      (itemconfigure *canvas* *center-line* "dash" ".")

      ;; score text
      (setf *score-text*
            (create-text *canvas* (/ *width* 2) 30 "0   :   0"))
      (itemconfigure *canvas* *score-text* "fill" "white")
      (itemconfigure *canvas* *score-text* "font" "Helvetica 24 bold")

      ;; left paddle
      (setf *left-paddle*
            (create-rectangle *canvas*
                              *left-paddle-x* *left-paddle-y*
                              (+ *left-paddle-x* *paddle-width*)
                              (+ *left-paddle-y* *paddle-height*)))
      (itemconfigure *canvas* *left-paddle* "fill" "white")

      ;; right paddle
      (setf *right-paddle*
            (create-rectangle *canvas*
                              *right-paddle-x* *right-paddle-y*
                              (+ *right-paddle-x* *paddle-width*)
                              (+ *right-paddle-y* *paddle-height*)))
      (itemconfigure *canvas* *right-paddle* "fill" "white")

      ;; ball
      (setf *ball*
            (create-oval *canvas*
                         *ball-x* *ball-y*
                         (+ *ball-x* *ball-size*)
                         (+ *ball-y* *ball-size*)))
      (itemconfigure *canvas* *ball* "fill" "white")

      ;; key bindings
      (bind root "<KeyPress-w>"
            (lambda (event)
              (declare (ignore event))
              (setf *w-pressed* t)))
      (bind root "<KeyRelease-w>"
            (lambda (event)
              (declare (ignore event))
              (setf *w-pressed* nil)))

      (bind root "<KeyPress-s>"
            (lambda (event)
              (declare (ignore event))
              (setf *s-pressed* t)))
      (bind root "<KeyRelease-s>"
            (lambda (event)
              (declare (ignore event))
              (setf *s-pressed* nil)))

      (bind root "<KeyPress-Up>"
            (lambda (event)
              (declare (ignore event))
              (setf *up-pressed* t)))
      (bind root "<KeyRelease-Up>"
            (lambda (event)
              (declare (ignore event))
              (setf *up-pressed* nil)))

      (bind root "<KeyPress-Down>"
            (lambda (event)
              (declare (ignore event))
              (setf *down-pressed* t)))
      (bind root "<KeyRelease-Down>"
            (lambda (event)
              (declare (ignore event))
              (setf *down-pressed* nil)))

      ;; initial state
      (setf *left-score* 0
            *right-score* 0
            *left-paddle-y* (/ (- *height* *paddle-height*) 2)
            *right-paddle-y* (/ (- *height* *paddle-height*) 2))
      (reset-ball)
      (update-score-text)
      (redraw)

      (game-loop)
      (mainloop))))

(start-game)
