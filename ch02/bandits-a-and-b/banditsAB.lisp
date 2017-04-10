;-*- Mode: Lisp; Package: (bandits :use (common-lisp ccl ut)) -*-

(defvar n)
(defvar epsilon .1)
(defvar alpha .1)
(defvar QQ*)
(defvar QQ)
(defvar n_a)
(defvar randomness)
(defvar max-num-tasks 2)
(defvar rbar)
(defvar timetime)

(defun setup ()
  (setq n 2)
  (setq QQ (make-array n))
  (setq n_a (make-array n))
  (setq QQ* (make-array (list n max-num-tasks) 
                       :initial-contents '((.1 .8) (.2 .9)))))


(defun init (algorithm)
  (loop for a below n do
        (setf (aref QQ a) (ecase algorithm
                           ((:rc :action-values) 0.0)
                           (:sl 0)
                           ((:Lrp :Lri) 0.5)))
        (setf (aref n_a a) 0))
  (setq rbar 0.0)
  (setq timetime 0))

(defun runs (task algorithm &optional (num-runs 2000) (num-steps 1000))
  "algorithm is one of :sl :action-values :Lrp :Lrp :rc"
  (standardize-random-state)
  (loop with average-reward = (make-list num-steps :initial-element 0.0)
        with prob-a* = (make-list num-steps :initial-element 0.0)
        with a* = (if (> (aref QQ* 0 task) (aref QQ* 1 task)) 0 1)
        for run-num below num-runs
        do (init algorithm)
        collect (loop for timetime-step below num-steps
                      for a = (policy algorithm)
                      for r = (reward a task)
                      do (learn algorithm a r)
                      do (incf (nth timetime-step average-reward) r)
                      do (when (= a a*) (incf (nth timetime-step prob-a*))))
        finally (return 
                 (loop for i below num-steps 
                       do (setf (nth i average-reward)
                                (/ (nth i average-reward)
                                   num-runs))
                       do (setf (nth i prob-a*)
                                (/ (nth i prob-a*)
                                   (float num-runs)))
                       finally (return (values average-reward prob-a*))))))

(defun policy (algorithm)
  (ecase algorithm
    ((:rc :action-values)
     (epsilon-greedy epsilon))
    (:sl 
     (greedy))
    ((:Lrp :Lri)
     (with-prob (aref QQ 0) 0 1))))
    

(defun learn (algorithm a r)
  (ecase algorithm
    (:rc
     (incf timetime)
     (incf rbar (/ (- r rbar)
                   timetime))
     (incf (aref QQ a) (- r rbar)))
    (:action-values 
     (incf (aref n_a a))
     (incf (aref QQ a) (/ (- r (aref QQ a))
                         (aref n_a a))))
    (:sl 
     (incf (aref QQ (if (= r 1) a (- 1 a)))))
    ((:Lrp :Lri)
     (unless (and (= r 0) (eq algorithm :Lri))
       (let* ((target-action (if (= r 1) a (- 1 a)))
              (other-action (- 1 target-action)))
         (incf (aref QQ target-action)
               (* alpha (- 1 (aref QQ target-action))))
         (setf (aref QQ other-action)
               (- 1 (aref QQ target-action))))))))

(defun reward (a task-num)
  (with-prob (aref QQ* a task-num)
    1 0))

(defun epsilon-greedy (epsilon)
  (with-prob epsilon 
    (random n)
    (arg-max-random-tiebreak QQ)))

(defun greedy ()
  (arg-max-random-tiebreak QQ))

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

(defun max-QQ* (num-tasks)
  (mean (loop for task below num-tasks 
              collect (loop for a below n 
                            maximize (aref QQ* a task)))))

