;;;The structure of this file is acrobot-window, pole dynamics stuff,
;;; acrobot-display stuff, top-level stuff, agents


;;; The acrobot-WINDOW is a basic simulation-window with just a few specializations.

(defclass acrobot-WINDOW 
  (stop-go-button step-button quiet-button simulation-window)
  ((world :accessor acrobot)))

(defmethod window-close :before ((window acrobot-window))
  (window-close (3D-graph-window (acrobot window))))

(defmethod view-draw-contents :after ((w acrobot-window))
  (when (and (slot-boundp w 'world) (slot-boundp (acrobot w) 'flip))
    (draw-acrobot-background (acrobot w))))

(defclass acrobot (terminal-world displayable-world) 
  ((acrobot-position1 :reader acrobot-position1 :initarg :acrobot-position1 :initform 0.0)
   (acrobot-velocity1 :accessor acrobot-velocity1 :initarg :acrobot-velocity1 :initform 0.0)
   (acrobot-position2 :reader acrobot-position2 :initarg :acrobot-position2 :initform 0.0)
   (acrobot-velocity2 :accessor acrobot-velocity2 :initarg :acrobot-velocity2 :initform 0.0)
   (side-view :accessor side-view :initarg :side-view)
   (phase-view1 :accessor phase-view1 :initarg :phase-view1)
   (phase-view2 :accessor phase-view2 :initarg :phase-view2)
   (3D-Graph-window :accessor 3D-graph-window)
   (last-action :accessor last-action :initform nil)
   white
   black
   flip
   fat-flip))

(defmethod world-state ((p acrobot))
  (list (acrobot-position1 p) (acrobot-velocity1 p) (acrobot-position2 p) (acrobot-velocity2 p)))

(defvar PI/2 (coerce (/ PI 2) 'long-float))
(defvar 2PI (coerce (* PI 2) 'long-float))
(defvar -PI (coerce (- PI) 'long-float))
(defvar acrobot-limit1 (coerce PI 'long-float))
(defvar acrobot-limit2 (coerce PI 'long-float))
(defvar acrobot-delta-t 0.1e0)              ; seconds between state updates
(defvar acrobot-max-velocity1 (coerce (/ (* .04 PI) .02) 'long-float))
(defvar acrobot-max-velocity2 (coerce (/ (* .09 PI) .02) 'long-float))
(defvar acrobot-max-force 2e0)
(defvar acrobot-gravity 9.8e0)
(defvar acrobot-mass1 1.0e0)
(defvar acrobot-mass2 1.0e0)
(defvar acrobot-length1 1.0e0)
(defvar acrobot-length2 1.0e0)
(defvar acrobot-length-center-of-mass1 0.5e0)
(defvar acrobot-length-center-of-mass2 0.5e0)
(defvar acrobot-inertia1 1.0e0)
(defvar acrobot-inertia2 1.0e0)

(setq PI/2 (coerce (/ PI 2) 'long-float))
(setq acrobot-limit1 (coerce PI 'long-float))
(setq acrobot-limit2 (coerce PI 'long-float))
(setq acrobot-delta-t 0.2e0)              ; seconds between state updates
(setq acrobot-max-velocity1 (coerce (/ (* .04 PI) .02) 'long-float))
(setq acrobot-max-velocity2 (coerce (/ (* .09 PI) .02) 'long-float))
(setq acrobot-max-force 1e0)
(setq acrobot-gravity 9.8e0)
(setq acrobot-mass1 1.0e0)
(setq acrobot-mass2 1.0e0)
(setq acrobot-length1 1.0e0)
(setq acrobot-length2 1.0e0)
(setq acrobot-length-center-of-mass1 0.5e0)
(setq acrobot-length-center-of-mass2 0.5e0)
(setq acrobot-inertia1 1.0e0)
(setq acrobot-inertia2 1.0e0)

(defmethod world-transition ((p acrobot) a)
  (let* ((substeps 4)
         (1/substeps (/ substeps)))
    (loop repeat substeps until (terminal-state? p) do
          (let* ((q2 (acrobot-position2 p))
                 (q2-dot (acrobot-velocity2 p))
                 (q1 (- (acrobot-position1 p) PI/2))
                 (q1-dot (acrobot-velocity1 p))
                 (force (* acrobot-max-force (max -1 (min a 1))))
                 (cos-q2 (cos q2))
                 (sin-q2 (sin q2))
                 (cos-q1+q2 (cos (+ q1 q2)))
                 (m1 acrobot-mass1)
                 (m2 acrobot-mass2)
                 (l1 acrobot-length1)
                 (lc1 acrobot-length-center-of-mass1)
                 (lc2 acrobot-length-center-of-mass2)
                 (d11 (+ (* m1 lc1 lc1)
                         (* m2 (+ (* l1 l1)
                                  (* lc2 lc2)
                                  (* 2 l1 lc2 cos-q2)))
                         acrobot-inertia1 acrobot-inertia2))
                 (d22 (+ (* m2 lc2 lc2)
                         acrobot-inertia2))
                 (d12 (+ (* m2 (+ (* lc2 lc2)
                                  (* l1 lc2 cos-q2)))
                         acrobot-inertia2))
                 (h1 (+ (- (* m2 l1 lc2 sin-q2 q2-dot q2-dot))
                        (- (* 2 m2 l1 lc2 sin-q2 q2-dot q1-dot))))
                 (h2 (* m2 l1 lc2 sin-q2 q1-dot q1-dot))
                 (phi1 (+ (* (+ (* m1 lc1) (* m2 l1))
                             acrobot-gravity (cos q1))
                          (* m2 lc2 acrobot-gravity cos-q1+q2)))
                 (phi2 (* m2 lc2 acrobot-gravity cos-q1+q2))
                 (q2-acc (/ (+ force 
                               (* d12 (/ d11) (+ h1 phi1))
                               (- h2)
                               (- phi2))
                            (- d22
                               (* d12 d12 (/ d11)))))
                 (q1-acc (/ (+ (* d12 q2-acc) h1 phi1)
                            (- d11))))
            (incf q1-dot (* 1/substeps acrobot-delta-t q1-acc))
            (bound q1-dot (* 2 acrobot-max-velocity1))
            (incf q1 (* 1/substeps acrobot-delta-t q1-dot))
            (incf q2-dot (* 1/substeps acrobot-delta-t q2-acc))
            (bound q2-dot (* 2 acrobot-max-velocity2))
            (incf q2 (* 1/substeps acrobot-delta-t q2-dot))
            ;(print (list q1 q1-dot q2 q2-dot))
            (set-acrobot-state p (+ q1 PI/2) q1-dot q2 q2-dot a))))
  (setf (world-reward p) -1)
  (world-reward p))

(defun acrobot-cm-angle (state)
  (let* ((q1 (first state))
         (q2 (third state))
         (m1 acrobot-mass1)
         (m2 acrobot-mass2)
         (l1 acrobot-length1)
         (lc1 acrobot-length-center-of-mass1)
         (lc2 acrobot-length-center-of-mass2)
         (x- (sin q2))
         (x (/ (* m2 x-)
               (+ m1 m2)))
         (y- (+ l1 lc2 (cos q2)))
         (y (/ (+ (* m1 lc1) (* m2 y-))
               (+ m1 m2))))
    (+ q1 (atan (/ x y)))))

(defmethod terminal-state? ((acrobot acrobot) &optional (state (world-state acrobot)))
;  (> (abs (acrobot-cm-angle x)) PI))
  (let* ((angle1 (first state))
         (angle2 (third state))
         (x (* acrobot-length1 (sin angle1)))
         (y (- (* acrobot-length1 (cos angle1))))
         (total-angle (+ angle1 angle2))
         (handx (+ x (* acrobot-length2 (sin total-angle))))
         (handy (+ y (- (* acrobot-length2 (cos total-angle))))))
  (and ;(> handx 1)
       ;(< handx 1.45)
       (> handy 1)
       )));(< handy 1.45))))

(defmethod world-reset ((world acrobot))
  (sleep .5)
  (set-acrobot-state world 0 0 0 0 nil)
  (print (world-time world))
  (when (window world)
    (let* ((window (window world))
           (black (g-color-black window))
           (white (g-color-white window)))
      (gd-fill-rect-r window 20 400 200 50 white)
      (gd-draw-text window 
                    (format nil "~A" (+ 1 (length (simulation-trial-reward-history window))))
                    '("monaco" :srcXor 24)
                    20 650 black)
      (gd-draw-text window "" '("chicago" :srcXor 12) 20 650 black)))
  (world-state world))

(defclass acrobot-phase-view2 (g-view) ())

(defmethod g-click-event-handler ((top-view acrobot-phase-view2) x y)
  (let ((state (list (acrobot-position1 *world*) (acrobot-velocity1 *world*) x y)))
    (format t "~A~%" (if (terminal-state? *world* state)
                             0
                             (state-value *agent* (sense *agent* *world* state))))))

(defclass acrobot-phase-view1 (g--view) ())

(defmethod g-click-event-handler ((top-view acrobot-phase-view1) x y)
  (let ((state (list x y (acrobot-position2 *world*) (acrobot-velocity2 *world*))))
    (format t "~A~%" (if (terminal-state? *world* state)
                             0
                             (state-value *agent* (sense *agent* *world* state))))))

(defmethod world-init-display ((acrobot acrobot))
  (with-slots (window side-view phase-view2 phase-view1) acrobot
    (unless (displayp acrobot)
      (setf window (make-instance 'acrobot-window
                     :window-type :document
                     :window-show nil
                     :view-font '("chicago" 12 :plain)
                     :window-title "Acrobot"
                     :window-do-first-click t
                     :gd-viewport-r '(10 40 540 580)))
      (setf (3D-graph-window acrobot) (make-instance '3D-graph-window
                                        :gd-viewport-r '(580 100 400 400)
                                        :window-show nil))
      (let ((button (make-instance 'button-dialog-item
                      :view-container window
                      :dialog-item-text "3D Graph Joint 1"
                      :dialog-item-action  
                      #'(lambda (item) (acrobot-3D-graph-button-action window item)))))
        (set-view-position-y-up button 160 3)
        (add-subviews window button))
      (let ((button (make-instance 'button-dialog-item
                      :view-container window
                      :dialog-item-text "3D Graph Joint 2"
                      :dialog-item-action  
                      #'(lambda (item) (acrobot-3D-graph-button-action window item)))))
        (gd-set-viewport button 340 3 nil nil)
        (add-subviews window button))
      (setf (world window) acrobot))
    (g-set-coordinate-system window 0 0 1 1)
    (setf side-view (make-instance 'g-view :parent window))
    (setf phase-view1 (make-instance 'acrobot-phase-view1 :parent window))
    (setf phase-view2 (make-instance 'acrobot-phase-view2 :parent window))
    (gd-set-viewport side-view 20 80 520 580)
    (gd-set-viewport phase-view2 270 40 520 290)
    (gd-set-viewport phase-view1 20 40 270 290)
    (let ((limit (+ acrobot-length2 acrobot-length2)))
      (g-set-coordinate-system side-view (- limit) (- limit) limit limit))
    (g-set-coordinate-system phase-view1 (- acrobot-limit1) (- acrobot-max-velocity1)
                  acrobot-limit1 acrobot-max-velocity1)
    (g-set-coordinate-system phase-view2 (- acrobot-limit2) (- acrobot-max-velocity2)
                  acrobot-limit2 acrobot-max-velocity2)
    (setf (slot-value acrobot 'white) (g-color-white side-view))
    (setf (slot-value acrobot 'black) (g-color-black side-view))
    (setf (slot-value acrobot 'fat-flip) (g-color-flip side-view))
    (setf (slot-value acrobot 'fat-flip) 
          (g-color-set-pen side-view (slot-value acrobot 'fat-flip) nil nil 2 2))
    (setf (slot-value acrobot 'flip) (g-color-flip side-view))))

(defmethod draw-acrobot-background ((p acrobot))
  (with-slots (side-view black white last-drawn-reward phase-view2 phase-view1) p
    (when (displayp p)
;      (g-outline-rect phase-view2 (- acrobot-limit2) (- acrobot-max-velocity2)
;                      acrobot-limit2 acrobot-max-velocity2 black)
;      (g-outline-rect phase-view1 (- acrobot-limit1) (- acrobot-max-velocity1)
;                      acrobot-limit1 acrobot-max-velocity1 black)
;      (g-fill-rect side-view 1 1 1.45 1.45 (g-color-name side-view :gray))
      (let ((limit (+ acrobot-length2 acrobot-length2)))
        (g-draw-line side-view (- limit) 1 limit 1 (g-color-name side-view :gray))
        (loop for y from (- limit) to limit  by (/ limit 10) do
              (g-draw-point side-view 0 y black)))
      (g-draw-disk side-view 0 0 .02 black)
      (let ((window (window p)))
        (gd-fill-rect-r window 20 650 200 50 white)
        (gd-draw-text window 
                      (format nil "~A" (+ 1 (length (simulation-trial-reward-history window))))
                      '("monaco" :srcXor 24)
                      20 650 black)
        (gd-draw-text window "" '("chicago" :srcXor 12) 20 650 black))
      (setf (world-time p) (world-time p))
      (draw-acrobot-state p))))

(defconstant radians-to-degrees (/ 360 PI 2))
(defconstant degrees-to-radians (/ PI 180))

(defmethod draw-acrobot-state ((p acrobot))
  (with-slots (side-view phase-view2 phase-view1 acrobot-position2 acrobot-position1 
                         acrobot-velocity2 acrobot-velocity1 last-action 
                         fat-flip flip black white) p
;    (g-draw-disk phase-view1 acrobot-position1 acrobot-velocity1 .1 flip)
;    (g-draw-point phase-view1 acrobot-position1 acrobot-velocity1 black)
;    (g-draw-disk phase-view2 (- (mod (+ acrobot-position2 PI) 2PI) PI) acrobot-velocity2 .07 flip)
;    (g-draw-point phase-view2 (- (mod (+ acrobot-position2 PI) 2PI) PI) acrobot-velocity2 black)
    (let* ((x (* acrobot-length1 (sin acrobot-position1)))
           (y (- (* acrobot-length1 (cos acrobot-position1))))
           (dx (gd-coord-x side-view x))
           (dy (gd-coord-y side-view y))
           (total-angle (+ acrobot-position1 acrobot-position2))
           (xinc (* acrobot-length2 (sin total-angle)))
           (yinc (- (* acrobot-length2 (cos total-angle))))
           (dradius 20)
           (arc-size 60)
           (fudge .2)
           (radius (g-offset-x side-view dradius)))
      (g-draw-line side-view 0 0 x y fat-flip)
      ;      (g-draw-disk side-view x y .04 flip)
      (g-draw-line-r side-view x y xinc yinc fat-flip)
      (gd-draw-arc side-view dx dy dradius 
                   (- (mod (truncate (* radians-to-degrees total-angle)) 360) 90)
                   (* arc-size (or last-action 0)) flip)
      (incf total-angle (* degrees-to-radians arc-size (or last-action 0)))
      (when (member last-action '(1 -1))
        (g-draw-arrowhead side-view (+ x (* fudge xinc)) (+ y (* fudge yinc))
                          (+ x (* radius (sin total-angle)))
                          (- y (* radius (cos total-angle)))
                          0.0 .3 flip)))))
                        

(defclass CMAC-acrobot-AGENT (acrobot-agent random-policy greedy-policy ERFA-Q-Learning) ())

(defmethod agent-step :after ((agent CMAC-acrobot-agent) x a y r)
  (declare (ignore x a y r))
  (when (update-displayp (world agent))
    (with-slots (side-view black white) (world agent)
      (let* ((base (+ 1 (gd-coord-y side-view 1.0)))
             (time (world-time (world agent)))
             (x (+ 20 (mod time 500)))
             (x+ (+ 20 (mod (+ time 15) 500)))
             (length (min 65 (truncate (* (slot-value agent 'a-value) 0.5)))))
        (gd-draw-line-r side-view x+ base 0 65 white)
        (gd-draw-point side-view x (- base length) black)))))

(defmethod set-acrobot-state ((p acrobot) new-acrobot-position1 new-acrobot-velocity1 
                              new-acrobot-position2 new-acrobot-velocity2 new-action)
  (when (update-displayp p) (draw-acrobot-state p))
  (setf (slot-value p 'acrobot-position1) new-acrobot-position1)
  (setf (slot-value p 'acrobot-velocity1) new-acrobot-velocity1)
  (setf (slot-value p 'acrobot-position2) new-acrobot-position2)
  (setf (slot-value p 'acrobot-velocity2) new-acrobot-velocity2)
  (setf (slot-value p 'last-action) new-action)
  (when (update-displayp p) (draw-acrobot-state p)))

  
;;;TOP-LEVEL STUFF:

;(defun make-acrobot-simulation (&optional (agent-class 'manual-acrobot-agent))
(defun make-acrobot-simulation (&optional (agent-class 'acrobot-sarsa-agent))
  (let ((acrobot (make-instance 'acrobot)))
    (setf (update-displayp acrobot) t)
    (setf (agent (window acrobot)) (make-agent acrobot agent-class))
    (when (typep (agent (window acrobot)) 'sarsa-agent)
      (setf (lambda (agent (window acrobot))) .9))))

(defun make-acrobot-and-run-silently (agent-class num-steps)
  (let* ((acrobot (make-instance 'acrobot))
         (agent (make-agent acrobot agent-class))
         (simulation (make-instance 'simulation :agent agent :world acrobot)))
    (simulation-run simulation num-steps)))
  
;;; AGENT STUFF BEGINS HERE

(defclass acrobot-AGENT (terminal-agent tabular-action-agent) ())

(defmethod make-agent ((acrobot acrobot) &optional (agent-class 'q-acrobot-agent))
  (cond ((subtypep agent-class 'q-acrobot-agent)
         (make-instance agent-class :num-actions 3))
        ((subtypep agent-class 'cmac-acrobot-agent) 
         (make-instance agent-class :world acrobot :num-actions 3))
        ((subtypep agent-class 'manual-agent) 
         (make-instance agent-class))))

(defmethod convert-action ((agent tabular-action-agent) (world acrobot) action-number)
  (- action-number 1))

;;; A Q-agent could be done as follows.  Divide the unit square of acrobot 
;;; positions and velocities into a large number of intervals, say 100
;;; for position and 10 for velocity.  Let 
;;; each one be a Q-learner state.  Consider 3 actions, +1, 0, and -1.

#|
(defclass Q-acrobot-AGENT (acrobot-agent random-policy tabular-q-learning greedy-policy)
  ((num-states :accessor num-states :initarg num-states :initform 1000)
   (initial-Q-value :accessor initial-Q-value :initarg :initial-Q-value :initform 1.0)))

(defmethod sense ((agent tabular-q-learning) (world acrobot) &optional (pos-and-vel-list (world-state world)))
  (let* ((pos (first pos-and-vel-list))
         (vel (second pos-and-vel-list))
         (position (max 0 (min 0.999999 (/ (- pos acrobot-min) 
                                           (- acrobot-max acrobot-min)))))
         (velocity (max 0 (min 0.999999 (+ 0.5 (/ vel
                                                 acrobot-max-velocity2
                                                 2.0))))))
    (+ (* 10 (floor (* 100 position)))
       (floor (* 10 velocity)))))
|#
(defclass acrobot-sarsa-agent 
  (single-CMAC-acrobot-AGENT ERFA-sarsa-agent) ())

(defclass multi-CMAC-acrobot-AGENT (CMAC-acrobot-AGENT) ())

(defclass single-CMAC-acrobot-AGENT (CMAC-acrobot-AGENT) ())

(defmethod initialize-instance ((agent single-CMAC-acrobot-agent) &rest initargs)
  (apply #'call-next-method agent initargs)
  (with-slots (representer FAs num-actions) agent
    (setf (alpha agent) 0.2e0)
    (setf (gamma agent) 1.0e0)
    (setf (prob-of-random-action agent) 0)
    (gc)
    (setf representer
          (make-instance 'CMAC-representer
            :input-descriptor 
            (list (list (truncate (* 1000000 (- acrobot-limit1)))
                        (truncate (* 1000000 (* acrobot-limit1 1.000e0)))
                        6)
                  (list (truncate (* 1000000 (- acrobot-max-velocity1)))
                        (truncate (* 1000000 (* acrobot-max-velocity1 1.333e0)))
                        7)
                  (list (truncate (* 1000000 (- acrobot-limit2)))
                        (truncate (* 1000000 (* acrobot-limit2 1.000e0)))
                        6)
                  (list (truncate (* 1000000 (- acrobot-max-velocity2)))
                        (truncate (* 1000000 (* acrobot-max-velocity2 1.333e0)))
                        7))
          :contraction 1.0
          :num-layers 10))
  (setf FAs
        (loop for a below num-actions
              collect (make-instance 'normalized-step-adaline
                        :num-inputs (num-outputs representer)
                        :initial-weight (coerce (/ 0.0 (num-layers representer)) 'long-float))))))

(defmethod initialize-instance ((agent multi-CMAC-acrobot-agent) &rest initargs)
  (apply #'call-next-method agent initargs)
  (with-slots (representer FAs num-actions) agent
    (setf (alpha agent) 0.2e0)
    (setf (gamma agent) 1.0e0)
    (setf (prob-of-random-action agent) 0)
    (gc)
    (setf representer
          (make-instance 'multi-representer
            :num-inputs 4
            :representers
            (let ((limits 
                   (list (list (- acrobot-limit1) (* acrobot-limit1 1.000e0))
                         (list (- acrobot-max-velocity1) (* acrobot-max-velocity1 1.333e0))
                         (list (- acrobot-limit2) (* acrobot-limit2 1.000e0))
                         (list (- acrobot-max-velocity2) (* acrobot-max-velocity2 1.333e0))))
                  (intervals '(6 7 6 7)))
              (loop for limits-i in limits do
                    (setf (first limits-i) (truncate (* 1000000 (first limits-i))))
                    (setf (second limits-i) (truncate (* 1000000 (second limits-i)))))
              (append (make-singleton-representers 'CMAC-representer limits intervals 3)
                      (make-doubleton-representers 'CMAC-representer limits intervals 2)
                      (make-representers 'CMAC-representer (combinations 4 3) limits intervals 3)
                      (make-representers 'CMAC-representer '((0 1 2 3)) limits intervals 12)))))
    (setf FAs
        (loop for a below num-actions
              collect (make-instance 'normalized-step-adaline
                        :num-inputs (num-outputs representer)
                        :initial-weight (coerce (/ 0.0 (num-layers representer)) 'long-float))))))

(defmethod sense ((agent CMAC-acrobot-agent) (world acrobot) 
                  &optional (state-list (world-state world)))
  (if (terminal-state? world state-list)
    :terminal-state
    (let ((array (make-array (length state-list))))
      (setf (aref array 0) (- (mod (+ (first state-list) PI) 2PI) PI))
      (setf (aref array 1) (limit (second state-list) acrobot-max-velocity1))
      (setf (aref array 2) (- (mod (+ (third state-list) PI) 2PI) PI))
      (setf (aref array 3) (limit (fourth state-list) acrobot-max-velocity2))
      (loop for i below (length state-list) do
            (setf (aref array i) (truncate (* 1000000 (aref array i)))))
      array)))

(defclass manual-acrobot-agent (manual-pole-agent) ())

(defun acrobot-3D-graph-button-action (world-window item)
  (when (or (equal "3D Graph Joint 1" (dialog-item-text item))
            (equal "3D Graph Joint 2" (dialog-item-text item)))
    (disable-buttons world-window)
    (let ((old-sim-running (simulation-runningp world-window))
          (text (dialog-item-text item)))
      (setf (simulation-runningp world-window) nil)
      (eval-enqueue `(progn (set-dialog-item-text ,item "Graphing..")
                            (acrobot-3d-graph ,world-window ,text 20 (3D-graph-window (world ,world-window)))
                            (set-dialog-item-text ,item ,text)
                            (enable-buttons ,world-window)
                            (when ,old-sim-running (simulation-run ,world-window)))))))


(defmethod acrobot-3d-graph ((world-window acrobot-window) text res 
                      &optional (3d-window (make-instance '3D-graph-window 
                                             :view-size #@(400 400)
                                             :view-position #@(500 50)
                                             :window-show nil)))
  (with-slots (data-array) 3D-window                             
    (setf data-array (make-array (list res res)))
    (cond
     ((equal text "3D Graph Joint 1") 
      (loop for i below res
            for pos = (* acrobot-limit1 2 (- (/ (+ i 0.5) res) 0.5)) do
            (loop for j below res
                for vel = (* acrobot-max-velocity1 2 (- (/ (+ j 0.5) res) 0.5))
                do (setf (aref data-array i j) 
                         (state-value (agent world-window) 
                                      (sense (agent world-window) (world world-window)
                                             (list pos vel 
                                                   (acrobot-position2 (world world-window))
                                                   (acrobot-velocity2 (world world-window)))))))))
     ((equal text "3D Graph Joint 2") 
      (loop for i below res
            for pos = (* acrobot-limit2 2 (- (/ (+ i 0.5) res) 0.5)) do
            (loop for j below res
                for vel = (* acrobot-max-velocity2 2 (- (/ (+ j 0.5) res) 0.5))
                do (setf (aref data-array i j) 
                         (state-value (agent world-window) 
                                      (sense (agent world-window) (world world-window)
                                             (list (acrobot-position1 (world world-window))
                                                   (acrobot-velocity1 (world world-window))
                                                   pos vel)))))))
     (t (error "Unrecognized button")))
    (g-make-visible 3D-window)
    (g::graph-surface 3D-window data-array)))

(defvar scaling .3)

(defun draw-bot (side-view action position1 position2 black)
    (let* ((x (* acrobot-length1 (sin position1)))
           (y (- (* acrobot-length1 (cos position1))))
           (dx (gd-coord-x side-view x))
           (dy (gd-coord-y side-view y))
           (total-angle (+ position1 position2))
           (xinc (* acrobot-length2 (sin total-angle)))
           (yinc (- (* acrobot-length2 (cos total-angle))))
           (dradius (round (* scaling 20)))
           (arc-size 60)
           (fudge .25)
           (radius (g-offset-x side-view dradius)))
      (g-draw-line side-view 0 0 x y black)
      (g-draw-line-r side-view x y xinc yinc black)
      (gd-draw-arc side-view dx dy dradius 
                   (- (mod (truncate (* radians-to-degrees total-angle)) 360) 90)
                   (* arc-size (or action 0)) black)
      (incf total-angle (* degrees-to-radians arc-size (or action 0)))
      (when (member action '(1 -1))
        (g-draw-arrowhead side-view (+ x (* fudge xinc)) (+ y (* fudge yinc))
                          (+ x (* radius (sin total-angle)))
                          (- y (* radius (cos total-angle)))
                          0.0 .3 black))))
#|
(defun segments ()
  (loop for (off start end) in segments
        for offset = (round (* scaling off)) do
        (gd-set-viewport c offset 10 (round (+ offset (* scaling 400))) (round (+ 10 (* scaling 300))))
        (cl))
  (loop for (off start end) in segments
        for offset = (round (* scaling off)) do
        (gd-set-viewport c offset 10 (round (+ offset (* scaling 400))) (round (+ 10 (* scaling 300))))
        (segment offset start end)))

(defun cl () (g-clear c))

(defun segment (offset start end)
  (gd-set-viewport c offset 10 (round (+ offset (* scaling 400))) (round (+ 10 (* scaling 300))))
  (g-draw-line c -1 1 +2 1 black)
  (g-draw-disk c 0 0 .02 black)
  (setq black (g-color-set-pen c black nil nil 2 2))
  (apply 'draw (nth start data))
  (setq black (g-color-set-size c black 1 1))
  (loop for n from start to end
        for d = (nth n data)
        do (apply 'draw d)))

(defun draw (a p1 p2) (draw-bot c (- a 1) p1 p2 black))


(setq segments '((1690 63 68) (1430 55 62) (1200 48 54) (975 41 47) (750 34 40) (540 28 33) (375 22 27) (200 16 21) (100 10 15) (0 4 9) (-70 0 3)))


(defun scrap-segments ()
    (start-picture c)
    (segments)
    (put-scrap :pict (get-picture c)))


(defun make-acrobot-and-run-trials-silently (agent-class num-trials num-steps)
  (let* ((acrobot (make-instance 'acrobot))
         (agent (make-agent acrobot agent-class))
         (simulation (make-instance 'simulation :agent agent :world acrobot)))
    (simulation-run-trials simulation num-trials num-steps)))
  
|#
