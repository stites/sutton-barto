;;; Gambler's problem.  The gambler has a stake s between 0 and 100.  At each 
;;; play he wagers an integer <= s.  He wins that much with prob p, else he
;;; loses that much.  If he builds his stake to 100 he wins (thus he never 
;;; wagers more than (- 100 s)); if his stake falls to 0 he loses.

;;; Thus, the stake s is the state the actions are the size of the bid.

;;; Here we implement value iteration.


(defvar V (make-array 101 :initial-element 0))
(setf (aref V 100) 1)
(defvar p .45)

(defun backup-action (s a)
  (+ (* p (aref V (+ s a)))
     (* (- 1 p) (aref V (- s a)))))

(defun vi (&optional (epsilon .00000001))
  "Value iteration to the criterion epsilon"
  (loop collect (loop for i from 1 to 99 collect (list i (aref v i)))
        while (< epsilon
                 (loop for s from 1 below 100
                       for old-V = (aref V s)
                       do (setf (aref V s)
                                (loop for a from 1 upto (min s (- 100 s))
                                      maximize (backup-action s a)))
                       maximize (abs (- old-V (aref V s)))))
        ))


(defun policy (s &optional (epsilon .0000000001))
  (loop with best-value = -1
        with best-action
        for a from 1 upto (min s (- 100 s))
        for this-value = (backup-action s a)
        do (when (> this-value (+ best-value epsilon))
             (setq best-value this-value)
             (setq best-action a))
        finally (return best-action)))
