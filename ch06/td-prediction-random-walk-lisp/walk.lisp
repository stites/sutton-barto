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

(defun setup (num-runs num-walks)
  (setq w (make-array n))
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
    (incf (aref w x) (* (aref alpha-array x) (- target (aref w x))))
    (loop for i below n 
          with error = (- target (aref w x))
          do (incf (aref w i) (* (aref alpha-array i) error (aref e i))))))

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

(defun explore (alpha-type-arg alpha-arg lambda-arg trace-type-arg forward?
                          &optional (number-type 'float))
  (setq alpha-type alpha-type-arg)
  (setq alpha alpha-arg)
  (setq lambda lambda-arg)
  (setq lambda (coerce lambda number-type))
  (setq alpha (coerce alpha number-type))
  (setq trace-type trace-type-arg)
  (record (stats (loop for walk-set in standard-walks
               do (init)
               do (loop repeat 100 do (loop for walk in walk-set do (if forward?
                                                  (process-walk walk)
                                                  (process-walk-backwards walk))))
               collect (residual-error)))))

(defun learning-curve (alpha-type-arg alpha-arg lambda-arg trace-type-arg 
                                 &optional (processing :forward) (initial-w-arg 0.5)
                                 (number-type 'float))
  (setq alpha-type alpha-type-arg)
  (setq alpha alpha-arg)
  (setq lambda lambda-arg)
  (setq lambda (coerce lambda number-type))
  (setq alpha (coerce alpha number-type))
  (setq trace-type trace-type-arg)
  (setq initial-w initial-w-arg)
  (multi-mean 
   (loop for walk-set in standard-walks
         do (init)
         collect (cons (residual-error)
                       (loop for walk in walk-set
                             do (ecase processing
                                  (:forward (process-walk walk))
                                  (:backward (process-walk-backwards walk))
                                  (:MC (process-walk-MC walk)))
                             collect (residual-error))))))

(defun batch-learning-curve-TD ()
  (setq alpha 0.01)
  (setq lambda 0.0)
  (setq trace-type :none)
  (setq initial-w -1)
  (multi-mean 
   (loop with last-w = (make-array n)
         for walk-set in standard-walks
         do (init)
         collect (loop for num-walks from 1 to (length walk-set)
                       for walk-subset = (firstn num-walks walk-set) do
                       (loop do (loop for i below n do (setf (aref last-w i) (aref w i)))
                             do (loop for walk in walk-subset
                                      do (process-walk walk))
                             until (> .0000001 (loop for i below n 
                                                  sum (abs (- (aref w i) (aref last-w i))))))
                       collect (residual-error)))))
