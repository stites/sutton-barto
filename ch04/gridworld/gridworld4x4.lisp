(defvar c)
(defvar V)
(defvar V-)
(defvar VV)
(defvar rows)
(defvar columns)
(defvar states)
(defvar gamma 1.0)
(defvar terminals)
(defvar Vk)

#|
;(setq c (g-init-context))
;(set-view-size c 500 700)

;(defun scrap-it ()
;  (start-picture c)
;  (truncate-last-values)
;  (vgrids 0 1 2 3 10 999)
;  (put-scrap :pict (get-picture c)))

(defun gd-draw-grid (context xbase ybase xinc yinc numx numy color)
;  (gd-fill-rect context xbase ybase (+ xbase (* xinc numx))
;                (+ ybase (* yinc numy)) (g-color-bw context 0))
  (let ((white (g-color-bw context 0.2)))
    (gd-draw-rect context xbase (+ ybase (* yinc (- numy 1))) xinc yinc white)
    (gd-draw-rect context (+ xbase (* xinc (- numx 1))) ybase xinc yinc white))
  (loop for i from 0 to numx
        for x from xbase by xinc
        do (gd-draw-vector context x ybase 0 (* numy yinc) color))
  (loop for i from 0 to numy
        for y from ybase by yinc
        do (gd-draw-vector context xbase y (* numx xinc) 0 color)))

(defun gd-draw-text-in-grid (context text x y xbase ybase xinc yinc 
                                       &optional (font-spec '("times" 12)))
  (gd-draw-text context
                text
                font-spec
                (+ xbase 3 (* x xinc))
                (+ ybase 4 (* (- 3 y) yinc))
                nil))

(defun Vgrid (context pos k)
  (let* ((xinc 25)
         (yinc 20)
         (numx columns)
         (numy rows)
         (yspace 25)
         (xbase 50)
         (ybase (- 700 (* (+ yspace (* yinc numy)) (+ pos 1)))))
    (gd-draw-grid context xbase ybase xinc yinc numx numy (g-color-bw context 1))
    (loop for r below rows do
          (loop for c below columns do
                (gd-draw-text-in-grid context (format-number (aref (aref Vk k) c r))
                                      c r xbase ybase xinc yinc)))
    (incf xbase (+ xbase (* xinc numx)))
    (gd-draw-grid context xbase ybase xinc yinc numx numy (g-color-bw context 1))
    (loop for state from 1 below (- states 1)
          do (multiple-value-bind (x y) (xy-from-state state)
               (setf (aref V state) (aref (aref Vk k) x y))))
    (loop for r below rows do
          (loop for c below columns do
                (gd-draw-policy context (greedy-policy (state-from-xy c r))
                                c r xbase ybase xinc yinc)))))

(defun gd-draw-policy (context actions x y xbase ybase xinc yinc)
  (let ((centerx (+ xbase (* x xinc) (truncate xinc 2)))
        (centery (+ ybase (* (- 3 y) yinc) (truncate yinc 2)))
        (xsize (truncate (* xinc 0.4)))
        (ysize (truncate (* yinc 0.4)))
        (bl (g-color-bw context 1)))
    (loop for a in actions do
          (case a
            (0 (gd-draw-arrow context centerx centery centerx (- centery ysize) bl))
            (1 (gd-draw-arrow context centerx centery (+ centerx xsize) centery bl))
            (2 (gd-draw-arrow context centerx centery centerx (+ centery ysize) bl))
            (3 (gd-draw-arrow context centerx centery (- centerx xsize) centery bl))))))
|#
                            
(defun greedy-policy (state)
  (if (member state terminals)
    nil
    (loop with bestQ = -10000.0 and bestas = nil
          for a below 4
          for Q = (full-backup state a)
          do (cond ((> Q bestQ)
                    (setq bestQ Q)
                    (setq bestas (list a)))
                   ((= Q bestQ)
                    (push a bestas)))
          finally (return bestas))))


(defun format-number (num)
  (cond ((null num)
         "  T")
        ((<= (abs num) 9.95)
         (format nil "~4,1F" num))
        (t
         (format nil "~4,0F" num))))

(defun Vgrids (&rest Ks)
  (loop for pos from 0 
        for k in Ks
        do (vgrid c pos k)))


(defun setup ()
  (setq terminals '(0 15))
  (setq rows 4)
  (setq columns 4)
  (setq states 16)
  (setq V (make-array states :initial-element 0.0))
  (setq V- (make-array states :initial-element 0.0))
  (setq VV (make-array (list rows columns)))
  (setq Vk (make-array 1000 :initial-contents 
                       (loop repeat 1000 collect (make-array (list rows columns)))))
  nil
)

(defun compute-V ()
  (loop for i below states do (setf (aref V i) 0.0))
  (loop for k below 1000
        do (loop for state from 1 below (- states 1)
                 do (setf (aref V- state)
                          (mean (loop for a below 4 collect
                                      (full-backup state a))))
                 do (multiple-value-bind (x y) (xy-from-state state)
                      (setf (aref (aref Vk k) x y) (aref V state))))
        do (ut::swap V V-))
  (loop for state below states do 
        (multiple-value-bind (x y) (xy-from-state state)
          (setf (aref VV y x) (aref V state))))
  (sfa VV))

(defun compute-V* ()
  (loop for i below states do (setf (aref V i) 0.0))
  (loop for k below 1000
        do (loop for x from 1 below (- states 1)
                 do (setf (aref V- x)
                          (loop for a below 4 maximize
                                      (full-backup x a)))
                 do (multiple-value-bind (x y) (xy-from-state x)
                      (setf (aref (aref Vk k) x y) (aref V x))))
        do (ut::swap V V-))
  (loop for state below states do 
        (multiple-value-bind (x y) (xy-from-state state)
          (setf (aref VV y x) (aref V state))))
  (sfa VV))



(defun sfa (array)
  "Show Floating-Point Array"
  (cond ((= 1 (array-rank array))
         (loop for e across array do (format t "~8,3F" e)))
        (t (loop for i below (array-dimension array 0) do
                 (format t "~%")
                 (loop for j below (array-dimension array 1) do
                       (format t "~8,3F" (aref array i j)))))))

(defun full-backup (x a)
  (let (r y)
    (cond ((off-grid x a)
           (setq r -1)
           (setq y x))
          (t
           (setq r -1)
           (setq y (next-state x a))))
    (+ r (* gamma (aref V y)))))

(defun off-grid (state a)
  (multiple-value-bind (x y) (xy-from-state state)
    (case a
      (0 (incf y) (>= y rows))
      (1 (incf x) (>= x columns))
      (2 (decf y) (< y 0))
      (3 (decf x) (< x 0)))))
      
(defun next-state (state a)
  (multiple-value-bind (x y) (xy-from-state state)
    (case a
      (0 (incf y))
      (1 (incf x))
      (2 (decf y))
      (3 (decf x)))
    (state-from-xy x y)))

(defun state-from-xy (x y)
      (+ y (* x columns)))

(defun xy-from-state (state)
  (truncate state columns))

(defun truncate-last-values ()
  (loop for state from 1 below (- states 1)
        do (multiple-value-bind (x y) (xy-from-state state)
             (setf (aref (aref Vk 999) x y) 
                   (round (aref (aref Vk 999) x y))))))

