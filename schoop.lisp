;;;; schoop.lisp

(in-package #:schoop)

(defconstant unit 20)

(defclass node ()
  ((x :initform 0
      :initarg :x
      :accessor x-pos)

   (y :initform 0
      :initarg :y
      :accessor y-pos)

   (color :initform sdl:*white*
          :initarg :color
          :accessor color)))

(defclass segment (node)
  ((child :accessor child)))

(defclass snake ()
  ((body :initform '()
         :accessor body)

   (direction :initform ':right
              :accessor direction)

   (head :initform (make-instance 'node :color sdl:*blue*)
         :accessor head)))

(defmethod draw-snake ((snake snake))
  ;; draw the head
  (let ((head (head snake)))
    (sdl:draw-box-* (x-pos head) (y-pos head)
                    unit unit
                    :color (color head))))

(defmethod move-snake ((snake snake ))
  (let ((direction (direction snake))
        (head-x (x-pos (head snake)))
        (head-y (y-pos (head snake))))
    (cond ((eq direction :right) (setf (x-pos (head snake)) (+ head-x  unit)))
          ((eq direction :left ) (setf (x-pos (head snake)) (- head-x  unit)))
          ((eq direction :up   ) (setf (y-pos (head snake)) (- head-y  unit)))
          ((eq direction :down ) (setf (y-pos (head snake)) (+ head-y  unit))))))



;;; PARAMETERS
(defparameter *snake* (make-instance 'snake))

(defparameter *current-time-delta* -1)

(defun initialize ()
  (setf *snake* (make-instance 'snake)))

(defun update ()
  (let ((direction (direction *snake*)))
    ;; move the snake in the relevant direction
    (move-snake *snake*))
  (sleep 0.1))

(defun draw ()
  (sdl:clear-display sdl:*black*)
  (draw-snake *snake*))

(defun start-game ()
  (sdl:with-init ()
    (sdl:window 640 480 :title-caption "SCHOOP")
    (initialize)
    (sdl:with-events ()
      (:quit-event () t)

      (:key-down-event ()
                       (when (sdl:key-down-p :sdl-key-escape) (sdl:push-quit-event))
                       (when (sdl:key-down-p :sdl-key-s) (setf (direction *snake*) :down))
                       (when (sdl:key-down-p :sdl-key-a) (setf (direction *snake*) :left))
                       (when (sdl:key-down-p :sdl-key-w) (setf (direction *snake*) :up))
                       (when (sdl:key-down-p :sdl-key-d) (setf (direction *snake*) :right)))

      (:idle ()
        (update)
        (draw)
        (sdl:update-display)))))

;;; could be made better

#|
(defun load-textures ()
  (let ((current-directory (asdf:system-source-directory :schoop)))
    (setf *character* (sdl:load-image
                       (merge-pathnames #P"assets/character.bmp" current-directory)))))
|#



