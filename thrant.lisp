(defpackage :ant-sim
  (:use :common-lisp
	:cl-log
	:lispbuilder-sdl)
  (:export :start))

(in-package :ant-sim)


(defparameter *thrants* nil
  "list of currently active thrants")
(defparameter *croods* nil
  "list of croods all over the map")
(defparameter *lines* nil
  "lines that the thrants have made")
(defparameter *pheros* nil
  "pheros either emitted by croods or dropped by thrants")

(defparameter *width* 800)
(defparameter *height* 600)
(defparameter *initial-croods* 10)
(defparameter *initial-thrants* 1)

(defparameter *crood-draw-radius* 2)
(defparameter *crood-color* sdl:*white*
  "Croods are shown as white")
(defparameter *crood-energy* 200)
(defparameter *crood-eat-distance* 15
  "How close a crood needs to be for a thrant to eat it")
(defparameter *thrant-starting-energy* 500)
(defparameter *thrant-size* 6)
(defparameter *movement-per-energy-unit* 2)
(defparameter *energy-usage-per-turn* '(10 5)
  "average energy used per turn, with an offset")
(defparameter *phero-drop-probability* 0.6
  "how likely is a thrant to drop a new phero on each turn")
(defparameter *phero-draw-radius* 1)
(defparameter *phero-color* sdl:*blue*
  "Pheros are shown as blue")
(defparameter *phero-use-radius* 8)
(defparameter *phero-use-probability* 0.5)
(defparameter *phero-emit-probability* 0.01)
(defparameter *phero-emit-radius* 10)
(defparameter *max-pheros-per-crood* 10)

(defcategory :info)
(defcategory :debug)
(defun log-cat (cat formatstr &rest args)
  (let ((message (apply #'format nil formatstr args)))
    (log-message cat message)))

(defstruct pos
  x y)

(defstruct line
  frompos topos col)

(defclass thrant ()
  ((position :accessor thrant-pos
	     :initarg :pos)
   (energy   :accessor thrant-energy
	     :initarg :energy)
   (color    :accessor thrant-col
	     :initarg :col)
   (inventory :accessor thrant-inv
	      :initarg :inventory
	      :initform nil)))

(defclass crood ()
  ((position :accessor crood-pos
	     :initarg :pos)
   (num-pheros :accessor crood-num-pheros
	       :initform 0)))

(defclass phero ()
  ((position :accessor phero-pos
	     :initarg :pos)))

(defun new-random-position ()
  (make-pos :x (random *width*) :y (random *height*)))

(defun new-thrant-color ()
  (flet ((comp ()
	   (+ (random 40) 210)))
    (sdl:color :r (comp) :g (comp) :b (comp))))
	       
(defun clear-lists ()
  (setf *croods* nil)
  (setf *thrants* nil)
  (setf *pheros* nil)
  (setf *lines* nil))

(defun add-random-croods ()
  (dotimes (i *initial-croods*)
    (let ((newcrood (make-instance 'crood :pos (new-random-position))))
      (push newcrood *croods*))))

(defun add-phero-pos-radial (centre-pos num)
  (let* ((angle (* num (/ 360.0 *max-pheros-per-crood*)))
	 (radius (+ *phero-emit-radius* (* num 2)))
	 (delta-x (round (* (cos angle) radius)))
	 (delta-y (round (* (sin angle) radius))))
    (make-pos :x (+ (pos-x centre-pos) delta-x)
	      :y (+ (pos-y centre-pos) delta-y))))

(defun maybe-emit-phero (crood)
  (when (and (< (random 1.0) *phero-emit-probability*)
	     (< (crood-num-pheros crood) *max-pheros-per-crood*))
    (push (make-instance
	   'phero :pos (add-phero-pos-radial
			(crood-pos crood)
			(crood-num-pheros crood)))
	  *pheros*)
    (incf (crood-num-pheros crood))))

(defun show-crood (crood)
  "A sall white circle"
  (sdl:draw-filled-circle-* (pos-x (crood-pos crood))
			    (pos-y (crood-pos crood))
			    *crood-draw-radius*
			    :color *crood-color*))

(defun add-random-thrants ()
  (dotimes (i *initial-thrants*)
    (let ((newthrant (make-instance 'thrant
				    :pos (new-random-position)
				    :col sdl:*red*
				    :energy *thrant-starting-energy*
				    :inventory nil)))
      (log-cat :debug "Created thrant at (~a, ~a)~%"
	   (pos-x (thrant-pos newthrant))
	   (pos-y (thrant-pos newthrant)))
      (push newthrant *thrants*))))

;; TODO(krato): Find a way to represent energy
(defun show-thrant (thrant)
  "A small colored square"
  (let ((p (thrant-pos thrant)))
    (sdl:draw-box-* (pos-x p) (pos-y p)
		    *thrant-size* *thrant-size*
		    :color (thrant-col thrant))))

(defun energy-usage (energy)
  (let* ((offset (random (second *energy-usage-per-turn*)))
	 (plus-or-minus (random 2))
	 (total-usage (+ (first *energy-usage-per-turn*)
			 (if (eq plus-or-minus 0)
			     offset
			     (- offset)))))
    (if (< total-usage energy)
	total-usage
	energy)))

(defun within-bounds (pos)
  (let ((x (pos-x pos))
	(y (pos-y pos)))
    (and (>= x 0)
	 (>= y 0)
	 (<= x *width*)
	 (<= y *height*))))

(defun choose-new-pos (old-pos move-size)
  "Pick one of four possible movements, if any one of them is close to
a phero, take it; otherwise pick one of the valid ones at random"
  (let* ((x-ratio (+ 0.5 (random 0.5)))
	 (delta-x (isqrt (round (* x-ratio (expt move-size 2)))))
	 (delta-y (isqrt (round (* (- 1 x-ratio) (expt move-size 2)))))
	 (x1 (+ (pos-x old-pos) delta-x))
	 (x2 (- (pos-x old-pos) delta-x))
	 (y1 (+ (pos-y old-pos) delta-y))
	 (y2 (- (pos-y old-pos) delta-y))
	 (pos-options (list (make-pos :x x1 :y y1)
			    (make-pos :x x1 :y y2)
			    (make-pos :x x2 :y y1)
			    (make-pos :x x2 :y y2)))
	 (valid-options (loop for pos in pos-options
			     when (within-bounds pos)
			     collect pos))
	 (cross-product nil))
    (progn
      (dolist (phero *pheros*)
	(dolist (opt valid-options)
	  (push (list opt phero (distance opt (phero-pos phero))) cross-product)))
      (let ((close-phero (car (sort cross-product #'< :key #'third))))
	(if (and (not (null close-phero))
		 (< (third close-phero) *phero-use-radius*)
		 (< (random 1.0) *phero-use-probability*))
	    (progn
	      (log-message :debug "Decided to follow a phero ...")
	      (delete (second close-phero) *pheros*)
	      (first close-phero))
	    (nth (random (length valid-options)) valid-options))))))

(defun maybe-drop-phero (pos)
  (if (< (random 1.0) *phero-drop-probability*)
      (log-message :debug "Decided to create a phero at ~a~%" pos)
      (push (make-instance 'phero :pos pos) *pheros*)))

(defun move-thrant (thrant)
  "Calculate movement amount and divide into x- and y- dimensions,
  then modify thrant accordingly"
  (let* ((energy-change (energy-usage (thrant-energy thrant)))
	 (move-size (* energy-change *movement-per-energy-unit*))
	 (old-pos (thrant-pos thrant))
	 (new-pos (choose-new-pos old-pos move-size)))
    (when (> energy-change 0)
      (log-message :debug "Thrant moved from ~a to ~a, it's energy is now ~a ~%"
	      old-pos new-pos (- (thrant-energy thrant) energy-change))
      (push (make-line :frompos old-pos :topos new-pos :col (thrant-col thrant))
	    *lines*)
      (setf (thrant-pos thrant) new-pos)
      (maybe-drop-phero new-pos)
      (decf (thrant-energy thrant) energy-change))))

(defun distance (pos1 pos2)
  (sqrt (+ (expt (- (pos-x pos1) (pos-x pos2)) 2)
	   (expt (- (pos-y pos1) (pos-y pos2)) 2))))

(defun eat-crood (thrant)
  "See if there is a crood nearby, and if so, harvest it!"
  (let* ((crood-distances (mapcar #'(lambda (crood)
				     (cons crood
					   (distance (crood-pos crood)
						     (thrant-pos thrant))))
				 *croods*))
	 (nearest-crood (car (sort crood-distances #'< :key #'cdr))))
    (when (< (cdr nearest-crood) *crood-eat-distance*)
      (log-message :info "Eating crood at ~a~%" (crood-pos (car nearest-crood)))
      (delete (car nearest-crood) *croods*)
      (log-message :debug "Number of croods left = ~a~%" (length *croods*))
      (incf (thrant-energy thrant) *crood-energy*)
      (log-message :debug "Thrant energy is now ~a!~%" (thrant-energy thrant)))))

(defun show-phero (phero)
  (sdl:draw-filled-circle-* (pos-x (phero-pos phero))
			    (pos-y (phero-pos phero))
			    *phero-draw-radius*
			    :color *phero-color*))

(defun show-line (line)
  (sdl:draw-line-* (pos-x (line-frompos line))
		   (pos-y (line-frompos line))
		   (pos-x (line-topos line))
		   (pos-y (line-topos line))
		   :color (line-col line)))

(defun start ()
  ;; Setup logging
  (log-manager)
  (start-messenger 'text-stream-messenger
		   :filter :info
		   :stream *standard-output*)
  ;; Initialize structures
  (clear-lists)
  (add-random-croods)
  (add-random-thrants)
  (sdl:with-init ()
    (sdl:window *width* *height*
		:title-caption "Ant Sim"
		:icon-caption "Ant Sim")
    (sdl:with-events ()
      (:quit-event () t)
      (:video-expose-event () (sdl:update-display))
      (:key-down-event (:key key)
		       (when (sdl:key= key :sdl-key-escape)
			 (sdl:push-quit-event)))
        (:mouse-button-down-event (:button button :x x :y y)
				  (let ((newcrood
					 (make-instance 'crood
							:pos (make-pos :x x :y y))))
				    (push newcrood *croods*)))
      (:idle ()
	     (progn
	       (sdl:clear-display sdl:*black*)
	       (dolist (crood *croods*)
		 (maybe-emit-phero crood)
		 (show-crood crood))
	       (dolist (line *lines*)
		 (show-line line))
	       (dolist (thrant *thrants*)
		 (show-thrant thrant)
		 (when (> (thrant-energy thrant) 0)
		   (move-thrant thrant))
		 (eat-crood thrant))
	       (dolist (phero *pheros*)
		 (show-phero phero))
	       (sleep 0.01) ;; TODO(krato): come up with something better!
	       (sdl:update-display))))))

