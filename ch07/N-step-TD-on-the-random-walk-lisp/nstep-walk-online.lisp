;-*- Package: (nstep) -*-

;;; A simulation of a TD(lambda) learning system to predict the expected outcome
;;; of a discrete-state random walk like that in the original 1988 TD paper.

;;; This version for n-step methods.  That n is the variable NN.
(defvar NN 1)


(defpackage :nstep
  (:use :common-lisp :g :ut :graph)
  (:nicknames :nstep))

(in-package :nstep)


(defvar n 5)                            ; the number of nonterminal states
(defvar w)                              ; the vector of weights = predictions
(defvar e)                              ; the eligibility trace
(defvar lambda .9)                      ; trace decay parameter
(defvar alpha 0.1)                      ; learning-rate parameter
(defvar standard-walks nil)             ; list of standard walks
(defvar targets)                        ; the correct predictions
(defvar right-outcome 1)
(defvar left-outcome -1)
(defvar initial-w 0.0)

(defun setup (num-runs num-walks)
  (setq w (make-array n))
  (setq e (make-array n))
  (setq standard-walks (standard-walks num-runs num-walks))
  (length standard-walks))

(defun init ()
  (loop for i below n do (setf (aref w i) initial-w))
  (setq targets 
        (loop for i below n collect 
              (+ (* (- right-outcome left-outcome) 
                    (/ (+ i 1) (+ n 1)))
                 left-outcome))))

(defun init-traces ()
  (loop for i below n do (setf (aref e i) 0)))

(defun learn (x target)
  (if (= lambda 0)
    (incf (aref w x) (* alpha (- target (aref w x))))
    (progn 
      (loop for i below n do (setf (aref e i) (* lambda (aref e i))))
      (incf (aref e x) 1)
      (loop for i below n 
            with error = (- target (aref w x))
            do (incf (aref w i) (* alpha error (aref e i)))))))

(defun process-walk (walk)
  (destructuring-bind (outcome states) walk
    (init-traces)
    (loop for s1 in states
          for s2 in (rest states)
          do (learn s1 (aref w s2)))
    (learn (first (last states)) outcome)))

(defun process-walk-nstep (walk)
  (destructuring-bind (outcome states) walk
   (loop for s1 in states
          for rest on states
          do (learn s1 (if (>= NN (length rest))
                         outcome
                         (aref w (nth NN rest)))))))

(defun standard-walks (num-sets-of-walks num-walks)
  (loop repeat num-sets-of-walks 
        with random-state = (ut::copy-of-standard-random-state)
        collect (loop repeat num-walks
                      collect (random-walk n random-state))))

(defun random-walk (n &optional (random-state *random-state*))
  (loop with start-state = (truncate (/ n 2))
        for x = start-state then (with-prob .5 (+ x 1) (- x 1) random-state)
        while (AND (>= x 0) (< x n))
        collect x into xs
        finally (return (list (if (< x 0) left-outcome right-outcome) xs))))

(defun residual-error ()
  "Returns the residual RMSE between the current and correct predictions"
  (rmse 0 (loop for w-i across w
                for target-i in targets
                collect (- w-i target-i))))

(defun learning-curve (alpha-arg lambda-arg)
  (setq alpha alpha-arg)
  (setq lambda lambda-arg)
  (multi-mean 
   (loop for walk-set in standard-walks
         do (init)
         collect (cons (residual-error)
                       (loop for walk in walk-set
                             do (process-walk walk)
                             collect (residual-error))))))

(defun learning-curve-nstep (alpha-arg NN-arg)
  (setq alpha alpha-arg)
  (setq NN NN-arg)
  (setq lambda 0)
  (multi-mean 
   (loop for walk-set in standard-walks
         do (init)
         collect (cons (residual-error)
                       (loop for walk in walk-set
                             do (process-walk-nstep walk)
                             collect (residual-error))))))

