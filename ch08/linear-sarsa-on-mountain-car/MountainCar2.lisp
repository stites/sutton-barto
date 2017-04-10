#|
This is an example program for reinforcement learning with linear function
approximation. The code follows the psuedo-code for linear, gradient-descent
Sarsa(lambda) given in Figure 8.8 of the book "Reinforcement Learning: An
Introduction", by Sutton and Barto. One difference is that we use the
implementation trick mentioned on page 189 to only keep track of the traces
that are larger than "min-trace". 

Before running the program you need to load the lisp tile-coding software,
available at http://www-anw.cs.umass.edu/~rich/tiles.lisp. If you are running
Macintosh Common Lisp then you will be able to see a graphical display of the
mountain car if you first load the G graphics package (see
http://www-anw.cs.umass.edu/~rich/G/g.html). This code can still be used without
graphics. After obtaining all the files, you load them: 

(load "tiles.lisp")
(load "ccl:library;quickdraw")      ;if necessary
(load "g.lisp")
(use-package :g)
(load "MountainCar.lisp")           ;this file

If you don't install the G graphics package, then there will be a lot of
complaints about undefined graphics routines, but (non-graphic) things should
still work as long as the call to "mcar-init-display" is commented out in the
function "setup". After loading, you need to call (setup), after which you will
be able to call (episode) to generate episodes of Mountain Car. (episode)
returns the length of the episode and prints it out. You can also call
"(3D-graph)" to get a display of the state-value function. Click on this to
update. 

The code below is in three main parts: 1) General RL code, 2) Mountain Car
code, and 3) code for the graphical display of the mountain car. Earlier parts
of the code can be understood independently of later parts. 

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

(defparameter N 8192)                  ; number of parameters in theta (memory-size)
(defparameter M 3)                      ; number of actions
(defparameter epsilon 0.01)              ; probability of random action
(defparameter alpha 0.5)                ; step-size parameter
(defparameter lambda 0.9)               ; trace-decay parameter
(defparameter gamma 1)                  ; discount-rate

(defun setup ()
  "Set everything up; zero out the agent's brain"
  (setq theta (make-array N :initial-element 0.0))
  (setq e (make-array N :initial-element 0.0))
  (setf nonzero-traces (make-array max-num-nonzero-traces))
  (setq num-nonzero-traces 0)
  (setf nonzero-traces-inverse (make-array N))
  (setq F (make-array M :initial-contents 
                      (loop repeat M collect (make-array num-tilings))))
  (setq Q (make-array M))
  ; comment out next line to nix display
  ;(mcar-init) (mcar-init-display) (3D-graph)
)

(defun episode (&optional (max-steps 10000))
  "the main function: runs one episode, returns length; see RLAI and figure 8.8"
  (mcar-init)                                     ; init mountain car
  (decay-traces 0)                                ; clear all traces
  (handler-case                                   ; to catch floating pt overflows
    (progn
      (load-F)                                    ; compute feature sets
      (loop for a below M do                      ; compute action values
            (setf (aref Q a) (compute-Q a)))
      (setq action (argmax Q))
      (when (with-probability epsilon) (setq action (random M)))
      (loop for time from 0 below max-steps do
            (decay-traces (* gamma lambda))       ; let traces fall
            (loop for a below M                   ; optionally clear other traces
                  when (not (= a action))
                  do (loop for i across (aref F a) do (clear-trace i)))
            (loop for i across (aref F action) do 
                  (set-trace i 1.0))              ; replacing traces
            (mcar-step action)                    ; do the action
            (setq r -1)                           ; get reward (always -1 in MC)
            (setq delta (- r (aref Q action)))
            (load-F)                              ; compute feature sets for new state
            (loop for a below M do                ; compute action values
                  (setf (aref Q a) (compute-Q a)))
            (setq action (argmax Q))
            (when (with-probability epsilon) (setq action (random M)))
            (unless (mcar-goal-p) (incf delta (* gamma (aref Q action))))
            (loop for i across nonzero-traces repeat num-nonzero-traces
                  with temp = (* delta (/ alpha num-tilings))
                  do (incf (aref theta i) (* temp (aref e i))))
            (setf (aref Q action) (compute-Q action))
            until (mcar-goal-p)
            finally (return (print time))))
    (floating-point-overflow (ignore) max-steps)))      ; return max on overflow

(defun compute-Q (a)
  "compute value of action for current F and theta"
    (loop for i across (aref F a) sum (aref theta i)))

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

;; Tile coding interface:

(defparameter num-tilings 10)           ; the number of tilings
(defparameter pos-width (/ 1.7 8))      ; the tile width for position
(defparameter vel-width (/ 0.14 8))     ; ...and velocity

(defparameter ctable (make-ct N :safe))

(defun load-F (&optional (F F) (pos mcar-position) (vel mcar-velocity))
  "compute feature sets for each action at current state"
  (loop for a below M
        with state-vars = (list (/ pos pos-width) (/ vel vel-width))
        do (load-tiles (aref F a) 0 num-tilings ctable state-vars (list a))))

;; Traces code:

#| Below is the code for selectively working only with traces >= min-trace. 
Other traces are forced to zero.  We keep a list of which traces are nonzero
so that we can work only with them.  This list is implemented as the array
"nonzero-traces" together with its length "num-nonzero-traces".  When a trace 
falls below min-trace and is forced to zero, we remove it from the list by 
decrementing num-nonzero-traces and moving the last element into the "hole"
in nonzero-traces made by this one that we are removing.  A final complication 
arises because sometimes we want to clear (set to zero and remove) a trace
but we don't know its position within the list of nonzero-traces.  To avoid
havint to search through the list we keep inverse pointers from each trace
back to its position (if nonzero) in the nonzero-traces list.  These inverse 
pointers are in the array "nonzero-traces-inverse". |#
 
(defvar nonzero-traces)                 ; array acting as list of traces > min-trace
(defvar num-nonzero-traces)             ; number of such "nonzero" traces
(defvar nonzero-traces-inverse)         ; map from feature back to its loc in list
(defparameter max-num-nonzero-traces 1000)   ; maximum length of list
(defparameter min-trace 0.01)                ; all traces below this are set to zero

(defun clear-trace (f)
  "Clears any trace for feature f"
  (unless (= (aref e f) 0)
    (clear-existent-trace f (aref nonzero-traces-inverse f))))

(defun clear-existent-trace (f l)
  "Clears the trace for feature f at location l in the list of nonzero traces"
  (setf (aref e f) 0)
  (decf num-nonzero-traces)
  (setf (aref nonzero-traces l)
        (aref nonzero-traces num-nonzero-traces))
  (setf (aref nonzero-traces-inverse (aref nonzero-traces l))
        l))

(defun decay-traces (decay-rate)
  "Decays all the (nonzero) traces by decay-rate, removing those below min-trace"
  (loop for l from (- num-nonzero-traces 1) downto 0
        for f = (aref nonzero-traces l)
        do
        (setf (aref e f) (* (aref e f) decay-rate))
        (when (< (aref e f) min-trace) (clear-existent-trace f l))))

(defun set-trace (f new-trace-value)
  "Set the trace for feature f to the given value, which must be positive"
  (cond ((>= (aref e f) min-trace)       ; trace already exists
         (setf (aref e f) new-trace-value))
        ((< num-nonzero-traces max-num-nonzero-traces)         ; room for one more
         (setf (aref e f) new-trace-value)
         (setf (aref nonzero-traces num-nonzero-traces) f)
         (setf (aref nonzero-traces-inverse f) num-nonzero-traces)
         (incf num-nonzero-traces))
        (t                              ; no room for new trace
         (increase-min-trace)
         (set-trace f new-trace-value))))

(defun increase-min-trace ()
  "Try to make room for more traces by incrementing min-trace by 10%, culling any traces that fall below the new minimum"
  (incf min-trace (* min-trace 0.1))
  (format t "~%Changing min-trace to ~A" min-trace)
  (loop for l from (- num-nonzero-traces 1) downto 0
        for f = (aref nonzero-traces l)
        when (< (aref e f) min-trace)
          do (clear-existent-trace f l)))

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

(defparameter pause-time 10)             ; msec to pause between display updates for
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
    (loop with F = (make-array M :initial-contents 
                      (loop repeat M collect (make-array num-tilings)))
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
                                    (loop for i across (aref F a) sum (aref theta i))))))))
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

