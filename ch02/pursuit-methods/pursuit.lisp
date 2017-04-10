(defvar n)
(defvar epsilon .1)
(defvar Q*)
(defvar Q)
(defvar p)
(defvar n_a)
(defvar randomness)
(defvar max-num-tasks 2000)

(defun setup ()
  (setq n 10)
  (setq Q (make-array n))
  (setq p (make-array n))
  (setq n_a (make-array n))
  (setq Q* (make-array (list n max-num-tasks)))
  (setq randomness (make-array max-num-tasks))
  (standardize-random-state)
  (advance-random-state 0)
  (loop for task below max-num-tasks do
        (loop for a below n do 
              (setf (aref Q* a task) (random-normal)))
        (setf (aref randomness task)
              (make-random-state))))

(defun init ()
  (loop for a below n do
        (setf (aref Q a) 0.0)
        (setf (aref P a) (/ 1.0 n))
        (setf (aref n_a a) 0)))

(defun runs (&optional (num-runs 1000) (num-steps 100) (beta 0))
  (loop with average-reward = (make-list num-steps :initial-element 0.0)
        with prob-a* = (make-list num-steps :initial-element 0.0)
        for run-num below num-runs
        for a* = 0
        do (format t " ~A" run-num)
        do (loop for a from 1 below n 
                 when (> (aref Q* a run-num)
                         (aref Q* a* run-num))
                 do (setq a* a))
        do (init)
        do (setq *random-state* (aref randomness run-num))
        do (loop for time-step below num-steps
                 for a = (policy)
                 for r = (reward a run-num)
                 do (learn a r beta)
                 do (incf (nth time-step average-reward) r)
                 do (when (= a a*) (incf (nth time-step prob-a*))))
        finally (loop for i below num-steps 
                      do (setf (nth i average-reward)
                               (/ (nth i average-reward)
                                  num-runs))
                      do (setf (nth i prob-a*)
                               (/ (nth i prob-a*)
                                  (float num-runs)))
                      finally (record num-runs num-steps "av-pursuit" 
                                      beta average-reward prob-a*))))

(defun policy ()
  (loop with rand = (random 1.0)
        for a below n
        sum (aref p a) into partial-sum
        until (>= partial-sum rand)
        finally (return a)))

(defun learn (a r beta)
  (incf (aref n_a a))
  (incf (aref Q a) (/ (- r (aref Q a))
                      (aref n_a a)))
  (loop for a below n
        do (decf (aref p a) (* beta (aref p a))))
  (incf (aref p (arg-max-random-tiebreak Q)) beta))

(defun reward (a task-num)
  (+ (aref Q* a task-num)
     (random-normal)))

(defun epsilon-greedy (epsilon)
  (with-prob epsilon 
    (random n)
    (arg-max-random-tiebreak Q)))

(defun greedy ()
  (arg-max-random-tiebreak Q))

(defun arg-max-random-tiebreak (array)
  "Returns index to first instance of the largest value in the array"
  (loop with best-args = (list 0)
        with best-value = (aref array 0)
        for i from 1 below (length array)
        for value = (aref array i) 
        do (cond ((< value best-value))
                 ((> value best-value)
                  (setq best-value value)
                  (setq best-args (list i)))
                 ((= value best-value)
                  (push i best-args)))
        finally (return (values (nth (random (length best-args))
                                     best-args)
                                best-value))))

(defun max-Q* (num-tasks)
  (mean (loop for task below num-tasks 
              collect (loop for a below n 
                            maximize (aref Q* a task)))))
