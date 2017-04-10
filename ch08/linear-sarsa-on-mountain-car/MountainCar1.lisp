#|
This is an example program for reinforcement learning with
linear function approximation. The code follows the
psuedo-code for linear, gradient-descent Sarsa(lambda) given
in Figure 8.8 of the book "Reinforcement Learning: An
Introduction", by Sutton and Barto. 

This version is kept simple, at the cost of efficiency.
Eligibility traces are implemented naively. Features sets are
lists. 

Before running the program you need to load the lisp
tile-coding software, available at
http://www-anw.cs.umass.edu/~rich/tiles.lisp. If you are running
Macintosh Common Lisp then you will be able to see a graphical
display of the mountain car if you first load the G graphics
package (see http://www-anw.cs.umass.edu/~rich/G/g.html). This
code can still be used without graphics. After obtaining all
the files, you load them: 

(load "tiles.lisp")
(load "ccl:library;quickdraw")      ;if necessary
(load "g.lisp")
(use-package :g)
(load "MountainCar.lisp")           ;this file

If you don't install the G graphics package, then there will
be a lot of complaints about undefined graphics routines, but
(non-graphic) things should still work as long as the call to
"mcar-init-display" is commented out in the function "setup".
After loading, you need to call (setup), after which you will
be able to call (episode) to generate episodes of Mountain
Car. (episode) returns the length of the episode and prints it
out. You can also call "(3D-graph)" to get a display of the
state-value function. Click on this to update. 

The code below is in three main parts: 1) General RL code, 2)
Mountain Car code, and 3) code for the graphical display of
the mountain car. Earlier parts of the code can be understood
independently of later parts. 

Written by Rich Sutton 12/17/00
|#

;;;;;;;;;;;;;;;;  General RL Code (see RLAI book)  ;;;;;;;;;;;;;;;;

(defvar theta)                          ; parameter vector
(defvar e)                              ; vector of eligibility traces
(defvar F)                              ; sets of features for each action
(defvar Q)                              ; array of action values
(defvar action)                         ; the action
(defvar r)                              ; the reward
(defvar delta)

(defparameter N 3000)                   ; number of parameters in theta
(defparameter M 3)                      ; number of actions
(defparameter epsilon 0.0)              ; probability of random action
(defparameter alpha 0.5)                ; step-size parameter
(defparameter lambda 0.9)               ; trace-decay parameter
(defparameter gamma 1)                  ; discount-rate

(defun setup ()
  "Set everything up; zero out the agent's brain"
  (setq theta (make-array N :initial-element 0.0))
  (setq e (make-array N))
  (setq F (make-array M))
  (setq Q (make-array M))
  ; comment out next line to nix display
  ;(mcar-init initial-state-randomness) (mcar-init-display) (3D-graph)
)

(defun episode (&optional (max-steps 10000))
  "the main function: runs one episode, returns length; see RLAI and figure 8.8"
  (mcar-init)                                     ; init mountain car
  (fill e 0)                                      ; clear all traces
  (handler-case                                   ; to catch floating pt overflows
    (progn
      (load-F)                                    ; compute feature sets
      (loop for a below M do                      ; compute action values
            (setf (aref Q a) (compute-Q a)))
      (setq action (argmax Q))
      (when (with-probability epsilon) (setq action (random M)))
      (loop for time from 0 below max-steps do
            (loop for i below N do                ; let traces fall
                  (setf (aref e i) (* (aref e i) gamma lambda)))
            (loop for a below M                   ; optionally clear other traces
                  when (not (= a action))
                  do (loop for i in (aref F a) do (setf (aref e i) 0)))
            (loop for i in (aref F action) do 
                  (setf (aref e i) 1.0))          ; replacing traces
            (mcar-step action)                    ; do the action
            (setq r -1)                           ; get reward (always -1 in MC)
            (setq delta (- r (aref Q action)))
            (load-F)                              ; compute feature sets for new state
            (loop for a below M do                ; compute new action values
                  (setf (aref Q a) (compute-Q a)))
            (setq action (argmax Q))
            (when (with-probability epsilon) (setq action (random M)))
            (unless (mcar-goal-p)
              (incf delta (* gamma (aref Q action))))
            (loop for i below N 
                  with temp = (* delta (/ alpha num-tilings))
                  do (incf (aref theta i) (* temp (aref e i))))
            (setf (aref Q action) (compute-Q action))
            until (mcar-goal-p)
            finally (return (print time))))
    (floating-point-overflow (ignore) max-steps)))      ; return max on overflow

(defun compute-Q (a)
  "compute value of action for current F and theta"
    (loop for i in (aref F a) sum (aref theta i)))

(defun argmax (Q)
  "returns index (action) of largest entry into Q array, breaking ties randomly"
  (loop with best-action = 0
        with best-value = (aref Q 0)
        with num-ties+1 = 1
        for a from 1 below M
        for value = (aref Q a) 
        do (cond ((< value best-value))
                 ((> value best-value)
                  (setq best-value value)
                  (setq best-action a))
                 (t                     ; a tie
                  (incf num-ties+1)
                  (when (= 0 (random num-ties+1))
                    (setq best-value value)
                    (setq best-action a))))
        finally return best-action))

;; Tile Coding Interface:

(defparameter num-tilings 10)           ; the number of tilings
(defparameter pos-width (/ 1.7 8))      ; the tile width for position
(defparameter vel-width (/ 0.14 8))     ; ...and velocity

(defun load-F (&optional (F F) (pos mcar-position) (vel mcar-velocity))
  "compute feature sets for each action at current state"
  (loop for a below M
        with state-vars = (list (/ pos pos-width) (/ vel vel-width))
        do (setf (aref F a)
                 (get-tiles state-vars num-tilings N a))))



;;;;;;;;;;;;;;  Mountain Car Code  ;;;;;;;;;;;;;;

(defvar mcar-position)
(defvar mcar-velocity)
(defvar mcar-last-action 1)

(defvar mcar-min-position -1.2)
(defvar mcar-max-position 0.6)
(defvar mcar-max-velocity 0.07)
(defvar mcar-goal-position 0.5)

(defparameter mcar-random-starts nil)   ; initial position random? or still at bottom? 

(defun mcar-init (&optional (random-state *random-state*))
  "Set car to its initial position for an episode"
  (without-event-processing
    (when mcar-window (mcar-draw-state))
    (setq mcar-position
          (if mcar-random-starts
            (random-in-interval mcar-min-position mcar-goal-position random-state)
            -0.5))
    (setq mcar-velocity 
          (if mcar-random-starts
            (random-in-interval (- mcar-max-velocity) mcar-max-velocity random-state)
            0.0))
    (setq mcar-last-action 1)
    (when mcar-window (mcar-draw-state))))

(defun mcar-step (a)
  "simulates mountain car for one step. a is 0, 1, or 2"
  (without-event-processing
    (when mcar-window (mcar-draw-state))
    (setq mcar-velocity (limit (+ mcar-velocity 
                                  (* .001 (ecase a (0 -1) (1 0) (2 +1)))
                                  (* -.0025 (cos (* 3 mcar-position)))) 
                               mcar-max-velocity))
    (setq mcar-position (+ mcar-position mcar-velocity))
    (bound2 mcar-position mcar-min-position mcar-max-position)
    (if (AND (= mcar-position mcar-min-position)
             (< mcar-velocity 0))
      (setq mcar-velocity 0))
    (setq mcar-last-action a)
    (when mcar-window (mcar-draw-state))))

(defun mcar-goal-p (&optional (pos mcar-position))
  "Is the car past the goal?"
  (>= pos mcar-goal-position))


;;; Now display stuff.  We make a window with top and side views.

(defparameter pause-time 0)             ; msec to pause between display updates for
                                        ; smoother animation (zero is fastest)
(defvar mcar-window nil)
(defvar mcar-top-view)
(defvar mcar-side-view)

(defvar *flip-color*)

(defclass mcar-window (g-window) ())    ; we are using G graphics package

(defmethod gd-click-event-handler ((w mcar-window) x y)
  (g-draw-view w))                      ; Redraw window on mouse clicks

(defmethod window-close :after ((w mcar-window))
  (setq mcar-window nil))               ; shutdown all graphics on closing the window

(defun mcar-init-display ()
  "initialize the mountain car display"
  (unless (AND mcar-window (wptr mcar-window))
    (without-interrupts
     (setq *flip-color* (g-color-flip t))
     (setq mcar-window (make-instance 'mcar-window 
                         :window-title "Mountain Car"
                         :grow-icon-p nil))
     (gd-set-viewport-r mcar-window nil nil 340 660)
     (g-set-cs mcar-window 0 20 340 660 :lower-left)   
     (setq mcar-top-view (make-instance 'g-view :parent mcar-window))
     (setq mcar-side-view (make-instance 'g-view :parent mcar-window))
     (g-set-viewport mcar-side-view 20 60 320 305)
     (g-set-viewport mcar-top-view 20 340 320 640)
     (g-set-cs mcar-side-view mcar-min-position -1.1 mcar-max-position 1.1)
     (g-set-cs mcar-top-view mcar-min-position (- mcar-max-velocity) 
               mcar-max-position mcar-max-velocity)))
  (g-make-visible mcar-window)
  (g-make-visible (target)))

(defmethod g-draw-view ((mcar-window mcar-window))
  "redraw entire main display"
  (g-clear mcar-window *blue-color*)
  (g-clear mcar-top-view 7084208);(g-color-rgb mcar-window 0.5 .2 .2))
  (g-fill-rect mcar-top-view 
               mcar-goal-position (- mcar-max-velocity)
               mcar-max-position mcar-max-velocity 
               *green-color*)
;  (g-outline-rect mcar-top-view mcar-min-position (- mcar-max-velocity) 
;                  mcar-max-position mcar-max-velocity *black-color*)
  (g-draw-line mcar-top-view mcar-min-position 0 mcar-max-position 0 *light-gray-color*)
  (loop with delta = 0.01 and radius = 0.09
        for pos from mcar-min-position to mcar-max-position by delta
        for next-pos from (+ mcar-min-position delta) to mcar-max-position by delta
        for slope = (mcar-slope pos)
        for angle = (atan slope)
        for last-x = nil then x
        for last-y = nil then y
        for x = (+ pos (* radius (sin angle)))
        for y = (- (mcar-height pos) (* radius (cos angle)))
        do (when last-x (g-draw-line mcar-side-view last-x last-y x y *white-color*)))
  (g-draw-line-r mcar-side-view mcar-goal-position 
                 (- (mcar-height mcar-goal-position) 0.09) 0 -.1 *white-color*)
  (mcar-draw-state))

(defun mcar-height (pos)
  (sin (* 3 pos)))

(defun mcar-slope (pos)
  (cos (* 3 pos)))

(defun mcar-draw-state ()
  "update state display"
  (let* ((d .1) 
         (h (mcar-height mcar-position)))
    (g-draw-disk mcar-top-view mcar-position mcar-velocity .033 *flip-color*)
    (g-draw-point mcar-top-view mcar-position mcar-velocity *light-gray-color*)
    (g-draw-disk mcar-side-view mcar-position h .07 *flip-color*)
    (case mcar-last-action
      (2  (g-draw-arrow mcar-side-view 
                        (+ mcar-position d) h (+ mcar-position d d) h *flip-color*))
      (0 (g-draw-arrow mcar-side-view 
                       (- mcar-position d) h (- mcar-position d d) h *flip-color*))))
  (pause pause-time))

(defun pause (msec) 
  "do some computing to make the machine pause"
  (loop repeat (* msec 10000) with x = 0 do (incf x)))

(defclass 3d-window (g-window) ;Additional display -- 3D graph of state-value function
  ((res :initform 30)
   (data-array :initform (make-array '(30 30)))))

(defvar 3D-window nil)

(defun 3d-graph (&optional (resolution 30))
  "Draws the state-value function in a 3d graph on a separate window"
  (without-interrupts
   (unless (AND 3D-window (wptr 3D-window))
     (setq 3D-window
           (make-instance '3d-window
             :window-title "State-Value Function (click to update)"
             :window-do-first-click nil
             :grow-icon-p nil))
     (gd-set-viewport-r 3D-window 500 50 400 400))
   (with-slots (res data-array) 3d-window
     (when (not (= res resolution))
       (setf res resolution)
       (setf data-array (make-array (list res res)))))
   (gd-click-event-handler 3d-window 0 0)))

(defmethod gd-click-event-handler ((3d-window 3d-window) x y)
  (3d-update))

(defun 3d-update ()
  (with-slots (res data-array) 3d-window
    (loop with F = (make-array M :initial-element nil)
          for i below res
          for pos = (+ mcar-min-position 
                       (* (- mcar-max-position mcar-min-position) (/ i (- res 1)))) 
          do (loop for j below res
                   for vel = (- (* 2 mcar-max-velocity (/ j (- res 1)))
                                mcar-max-velocity)
                   do (load-F F pos vel)
                   do (setf (aref data-array i j) 
                            (if (mcar-goal-p pos)
                              0.0
                              (loop for a below M maximizing
                                    (loop for i in (aref F a) sum (aref theta i))))))))
  (g-draw-view 3d-window))

(defmethod g-draw-view ((3d-window 3d-window))
  (g::graph-surface 3D-window (slot-value 3d-window 'data-array)))


;;;;;;  Some utilities  ;;;;;;;;;;

(defmacro bound2 (x limit1 limit2)
  "forces x to be within the given range"
  `(setf ,x (max ,limit1 (min ,limit2 ,x))))

(defmacro limit (x limit)
  `(max (- ,limit) (min ,limit ,x)))

(defmacro limit2 (x limit1 limit2)
  "returns x after being limited to the given range"
  `(max ,limit1 (min ,limit2 ,x)))

(defun random-in-interval (min max &optional (random-state *random-state*))
  (+ min (* (random 1.0 random-state) (- max min))))

(defun with-probability (p &optional (state *random-state*))
  (> p (random 1.0 state)))

