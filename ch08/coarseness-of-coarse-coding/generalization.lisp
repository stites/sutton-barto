; This code illustrates linear generalization in a 1D case.  The features 
; correspond to patches of the 0-1 interval.  We vary the width of the patches 
; to see the effect on generalization after various amounts of training.

; In particular, we image a case with 100 patches, each centered .01 apart,
; and with widths of either 3, 10 or 30 (or whatever).  For each we train on
; 100 input points also distributed uniformly over the interval.  The target
; function is a step function from 0 to 1 at the halfway point across the 
; interval.  We plot the resultant estimated function after various numbers
; of passes through the training set.

(defvar w)                              ; the weights
(defvar deltaw)                         ; the change in the weights
(defvar n)                              ; number of patches (100)
(defvar widths)                         ; the list of patch widths
(defvar width)                          ; the current width
(defvar alpha)                          ; step size

(defvar random-state (make-random-state))

(defun setup ()
  (setq alpha 0.2)
  (setq n 100)
  (setq w (make-array n))
  (setq deltaw (make-array n))
  (setq widths '(3 9 27)))

(defun init ()
  (loop for i below n do
        (setf (aref w i) 0.0)))

(defun f (x)
  (loop for i below n
        when (within-patch x i) sum (aref w i)))

(defun targetf (x)
  (if (and (>= x .4) (< x .6)) 1.0 0.0))

(defun within-patch (x i &optional (width width))
  (<= (abs (- x (/ i (float n)))) (/ width 2.0 n)))

(defun train ()
  (loop for j below n
        with x = (+ .25 (random 0.5 random-state))
        with target  = (targetf x)
        with f = (f x)
        with alpha-error = (* (/ alpha width) (- target f))
        when (within-patch x j) do (incf (aref w j) alpha-error)))

(defun show-f (context xbase ybase xscale yscale)
  (loop for i from (/ n 4) below (- n (/ n 4))
        for lastix = nil then ix
        for x = (/ i (float n))
        for ix = (round (+ xbase (* x xscale)))
        for lastiy = nil then iy
        for y = (f x)
        for iy = (round (+ ybase (* y yscale)))
        with color = (g-color-name context :black)
        when lastix do
        ;(print (list i x ix lastix y iy lastiy))
        (gd-draw-lineseg context lastix lastiy ix iy color)))

(defun show-target (context xbase ybase xscale yscale)
  (loop for i from (/ n 4) below (- n (/ n 4))
        for lastix = nil then ix
        for x = (/ i (float n))
        for ix = (round (+ xbase (* x xscale)))
        for lastiy = nil then iy
        for y = (targetf x)
        for iy = (round (+ ybase (* y yscale)))
        with color = (g-color-name context :black)
        when lastix do
        ;(print (list i x ix lastix y iy lastiy))
        (gd-draw-lineseg context lastix lastiy ix iy color)))

(defun sequence (context xbase ybase offset 
                         width-arg alpha-arg num-examples-list)
  (setq width width-arg)
  (setq alpha alpha-arg)
  (init)
  (gd-fill-rect context (+ xbase 100) (+ ybase 200) 
                (+ xbase 300) (- ybase (* (length num-examples-list) offset) 200)
                (g-color-name context :white))
  (show-target context xbase ybase 400 80)
  (loop for num-examples in num-examples-list
        for i from 1 
        do
        (loop repeat num-examples do (train))
        (show-f context xbase (- ybase (* i offset)) 400 80)))

(defun figure (context)
  (let ((examples '(10 30 120 480 1920 7680 30720));50 150 600 2400 9600));40 40 80 160 320 640 1280 2560)); 5120 10240))
        (ybase 640)
        (offset 80)
        (alph .2)
        (advances 0))
    (loop for n in examples
          for y from (- ybase offset) by (- offset)
          sum n into sum
          do (gd-draw-text context (format nil "~A" sum)
                           '("Helvetica" 24) 0 y (g-color-name context :black)))
    (standardize-random-state random-state) (advance-random-state advances random-state)
    (sequence context 0 ybase offset 3 alph examples)
    (standardize-random-state random-state) (advance-random-state advances random-state)
    (sequence context 300 ybase offset 9 alph examples)
    (standardize-random-state random-state) (advance-random-state advances random-state)
    (sequence context 600 ybase offset 27 alph examples)))


(defun scrap-figure (c)
    (start-picture c)
    (figure c)
    (put-scrap :pict (get-picture c)))
