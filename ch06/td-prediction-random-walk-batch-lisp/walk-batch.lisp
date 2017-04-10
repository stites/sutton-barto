;-*- Package: (discrete-walk) -*-

;;; A simulation of a TD(lambda) learning system to predict the expected outcome
;;; of a discrete-state random walk like that in the original 1988 TD paper.


(defpackage :discrete-walk
  (:use :common-lisp :g :ut :graph)
  (:nicknames :dwalk))

(in-package :dwalk)


(defvar n 5)                            ; the number of nonterminal states
(defvar w)                              ; the vector of weights = predictions
(defvar e)                              ; the eligibility trace
(defvar lambda .9)                      ; trace decay parameter
(defvar alpha 0.1)                      ; learning-rate parameter
(defvar initial-w 0.5)
(defvar standard-walks nil)             ; list of standard walks
(defvar trace-type :none)               ; :replace, :accumulate, :average, :1/t or :none
(defvar alpha-type :fixed)              ; :fixed, :1/t, or :1/t-max
(defvar alpha-array)                    ; used when each state has a different alpha
(defvar u)                              ; usage count = number of times updated
(defvar delta-w)

(defun setup (num-runs num-walks)
  (setq w (make-array n))
  (setq delta-w (make-array n))
  (setq e (make-array n))
  (setq u (make-array n))
  (setq alpha-array (make-array n))
  (setq standard-walks (standard-walks num-runs num-walks))
  (length standard-walks))

(defun init ()
  (loop for i below n do (setf (aref w i) initial-w))
  (loop for i below n do (setf (aref alpha-array i) alpha))
  (loop for i below n do (setf (aref u i) 0)))

(defun init-traces ()
  (loop for i below n do (setf (aref e i) 0)))

(defun learn (x target)
  (ecase alpha-type
    (:1/t (incf (aref u x))
          (setf (aref alpha-array x) (/ 1.0 (aref u x))))
    (:fixed)
    (:1/t-max (when (<= (aref u x) (/ 1 alpha))
                (incf (aref u x)) 
                (setf (aref alpha-array x) (/ 1.0 (aref u x))))))
  (ecase trace-type
    (:none)
    (:replace (loop for i below n do (setf (aref e i) (* lambda (aref e i))))
              (decf (aref u x) (aref e x))
              (setf (aref e x) 1))
    (:accumulate (loop for i below n do (setf (aref e i) (* lambda (aref e i))))
                 (incf (aref e x) 1))
    (:average (loop for i below n do (setf (aref e i) (* lambda (aref e i))))
              (setf (aref e x) (+ 1 (* (aref e x) (- 1 (aref alpha-array x))))))
    (:1/t (incf (aref u x))
          (incf (aref e x) 1)
          (loop for i below n 
                for lambda = (float (/ (aref u x)))
                do (setf (aref e i) (* lambda (aref e i))))))
  (if (eq trace-type :none)
    (incf (aref delta-w x) (* alpha (- target (aref w x))))
    (loop for i below n 
          with error = (- target (aref w x))
          do (incf (aref delta-w i) (* (aref alpha-array i) error (aref e i))))))

(defun process-walk (walk)
  (destructuring-bind (outcome states) walk
    (unless (eq trace-type :none) (init-traces))
    (loop for s1 in states
          for s2 in (rest states)
          do (learn s1 (aref w s2)))
    (learn (first (last states)) outcome)))

(defun process-walk-backwards (walk)
  (destructuring-bind (outcome states) walk
    (unless (eq trace-type :none) (init-traces))
    (learn (first (last states)) outcome)
    (loop for s1 in (reverse (butlast states))
          for s2 in (reverse (rest states))
          do (learn s1 (aref w s2)))))

(defun process-walk-MC (walk)
  (destructuring-bind (outcome states) walk
    (loop for s in (reverse states)
          do (learn s outcome))))

(defun standard-walks (num-sets-of-walks num-walks)
  (loop repeat num-sets-of-walks 
        with random-state = (ut::copy-of-standard-random-state)
        collect (loop repeat num-walks
                      collect (random-walk n random-state))))

(defun random-walk (n &optional (random-state *random-state*))
  (loop with start-state = (round (/ n 2))
        for x = start-state then (with-prob .5 (+ x 1) (- x 1) random-state)
        while (AND (>= x 0) (< x n))
        collect x into xs
        finally (return (list (if (< x 0) 0 1) xs))))

(defun residual-error ()
  "Returns the residual RMSE between the current and correct predictions"
  (rmse 0 (loop for i below n
                when (>= (aref w i) -.1)
                collect (- (aref w i)
                           (/ (+ i 1) (+ n 1) )))))

(defun batch-exp ()
  (setq lambda 0.0)
  (setq trace-type :none)
  (setq initial-w -1)
  (loop for walk-set in standard-walks
        for run-num from 0
        do (loop for l in '(0 1) do
                 (init)
                 (record l run-num
                         (loop for num-walks from 1 to (length walk-set)
                               for walk-subset = (firstn num-walks walk-set) do
                               (setf alpha (/ 1.0 n num-walks 3))
                               (loop do (loop for i below n do (setf (aref delta-w i) 0))
                                     do (loop for walk in walk-subset
                                              do (ecase l
                                                   (0 (process-walk walk))
                                                   (1 (process-walk-mc walk))))
                                     do (loop for i below n do (incf (aref w i) (aref delta-w i)))
                                     until (> .0000001 (loop for i below n 
                                                             sum (abs (aref delta-w i)))))
                               collect (residual-error))))))
