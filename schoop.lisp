;;;; schoop.lisp

(in-package #:schoop)

(defconstant unit 20)
(defconstant screen-width 640)
(defconstant screen-height 480)
(defconstant unit-width (/ screen-width unit))
(defconstant unit-height (/ screen-height unit))

(defun units (x)
  (* x unit))

;;; CLASSES & INSTANCE METHODS ---------------------------------------------------------------------
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

(defmethod draw-node ((node node))
  (sdl:draw-box-* (units (x-pos node)) (units (y-pos node))
                  unit unit
                  :color (color node)))

(defmethod draw-snake ((snake snake))
  (mapc #'draw-node (body snake))
  (draw-node (head snake)))

(defmethod move-snake ((snake snake))
  (let ((direction (direction snake))
        (head-x (x-pos (head snake)))
        (head-y (y-pos (head snake)))
        (previous-x -100)
        (previous-y -100))

    (unless *just-ate-apple*
      (loop for segment in (body snake) do (progn
                                             (let ((current-x (x-pos segment))
                                                   (current-y (y-pos segment)))
                                               (if (= previous-x -100)
                                                   (progn
                                                     (setf (x-pos segment) (x-pos (head snake)))
                                                     (setf (y-pos segment) (y-pos (head snake))))
                                                   (progn
                                                     (setf (x-pos segment) previous-x)
                                                     (setf (y-pos segment) previous-y)))
                                               (setf previous-x current-x)
                                               (setf previous-y current-y)))))

    (cond ((eq direction :right) (setf (x-pos (head snake)) (+ head-x  1)))
          ((eq direction :left ) (setf (x-pos (head snake)) (- head-x  1)))
          ((eq direction :up   ) (setf (y-pos (head snake)) (- head-y  1)))
          ((eq direction :down ) (setf (y-pos (head snake)) (+ head-y  1))))

    (setf *just-ate-apple* nil)))

;;; ------------------------------------------------------------------------------------------------
;;; PARAMETERS -------------------------------------------------------------------------------------

(defparameter *snake* (make-instance 'snake))

(defparameter *apple* :uninitialized)

(defparameter *current-time-delta* -1)

(defparameter *just-ate-apple* NIL)

;;; ------------------------------------------------------------------------------------------------
;;; FUNCTIONS --------------------------------------------------------------------------------------

(defun get-grid ()
  (let ((grid (make-array (list unit-width unit-height) :initial-element :empty))
        (head (head *snake*))
        (body (body *snake*)))
    ;; add the head
    (set-grid-element grid (x-pos head) (y-pos head) :head)

    ;; add the body segments
    (loop for seg in body do (set-grid-element grid (x-pos seg) (y-pos seg) :segment))

    ;; add the apple
    (unless (eql *apple* :uninitialized)
      (set-grid-element grid (x-pos *apple*) (y-pos *apple*) :apple))

    ;; done!
    (return-from get-grid grid)))

(defun spawn-apple ()
  (let* ((empty (get-empty-positions (get-grid)))
         (position (nth (random (length empty)) empty)))
    (setf *apple* (make-instance 'node :x (first position)
                                       :y (second position)
                                       :color sdl:*red*))))

(defun get-empty-positions (grid)
  (let ((empty-list '()))
    (dotimes (x (array-dimension grid 0) empty-list)
      (dotimes (y (array-dimension grid 1))
        (when (eql :empty (aref grid x y))
          (push `(,x ,y) empty-list))))))

(defun set-grid-element (grid x y item)
  (setf (aref grid x y) item))

(defun eat-apple ()
  (let* ((head (head *snake*))
         (new-segment (make-instance 'segment :x (x-pos head)
                                              :y (y-pos head)
                                              :color sdl:*green*)))
    (push new-segment (body *snake*))
    (spawn-apple)
    (setf *just-ate-apple* T)))

(defun initialize ()
  (setf *snake* (make-instance 'snake))
  (spawn-apple))

(defun update ()
  ;; move the snake in the relevant direction

  (let ((current-time (get-internal-real-time)))
    (if (< current-time *current-time-delta*)
        (return-from update)
        (setf *current-time-delta* (+ 100 current-time))))

  (move-snake *snake*)

  (let ((head (head *snake*)))
    ;; check to see if the snake has collided with the wall
    (when (or (< (x-pos head) 0)
              (> (x-pos head) (1- unit-width))
              (< (y-pos head) 0)
              (> (y-pos head) (1- unit-height)))
      (initialize))

    ;; check to see if the snake has collided with the apple
    (when (and (= (x-pos head) (x-pos *apple*))
               (= (y-pos head) (y-pos *apple*)))
      (eat-apple))

    ;; check to see if the snake has collided with itself
    (block body-collision
      (loop for segment in (body *snake*)
            do (when (and (= (x-pos segment) (x-pos head))
                          (= (y-pos segment) (y-pos head))
                          (not *just-ate-apple*))
                 (initialize)
                 (return-from body-collision))))))

(defun draw ()
  (sdl:clear-display sdl:*black*)
  (draw-snake *snake*)
  (draw-node *apple*))

(defun handle-input ()
  ;; quit on escape
  (when (sdl:key-down-p :sdl-key-escape) (sdl:push-quit-event))

  ;; move down
  (when (and (not (eq (direction *snake*) :up)) (sdl:key-down-p :sdl-key-s))
    (setf (direction *snake*) :down))

  ;; move up
  (when (and (not (eq (direction *snake*) :down)) (sdl:key-down-p :sdl-key-w))
    (setf (direction *snake*) :up))

  ;; move left
  (when (and (not (eq (direction *snake*) :right)) (sdl:key-down-p :sdl-key-a))
    (setf (direction *snake*) :left))

  ;; move right
  (when (and (not (eq (direction *snake*) :left)) (sdl:key-down-p :sdl-key-d))
    (setf (direction *snake*) :right)))

(defun start-game ()
  (sdl:with-init ()
    (sdl:window 640 480 :title-caption "SCHOOP"
                        :resizable NIL)
    (initialize)
    (sdl:with-events ()
      (:quit-event () t)

      (:key-down-event () (handle-input))

      (:idle ()
        (update)
        (draw)
        (sdl:update-display)))))
