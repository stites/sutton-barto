;;; Jack's car rental problem.  The state is n1 and n2, the number of cars
;;; at each location a the end of the day, at most 20.  Actions are numbers of cars 
;;; to switch from location 1 to location 2, a number between -5 and +5.

;;; P1(n1,new-n1) is a 26x21 array giving the probability that the number of cars at 
;;; location 1 is new-n1, given that it starts the day at n1.  Similarly for P2

;;; R1(n1) is a 26 array giving the expected reward due to satisfied requests at 
;;; location, given that the day starts with n1 cars at location 1.  SImilarly for R2.


;;; Here we implement policy iteration.


(defvar V)
(defvar lambda-requests1 3)
(defvar lambda-requests2 4)
(defvar lambda-dropoffs1 3)
(defvar lambda-dropoffs2 2)
(defvar policy)
(defvar P1)
(defvar P2)
(defvar R1)
(defvar R2)
(defvar gamma .9)
(defvar theta .0000001)

(defun setup ()
  (setq V (make-array '(21 21) :initial-element 0))
  (setq policy (make-array '(21 21) :initial-element 0))
  (setq P1 (make-array '(26 21) :initial-element 0))
  (setq P2 (make-array '(26 21) :initial-element 0))
  (setq R1 (make-array 26 :initial-element 0))
  (setq R2 (make-array 26 :initial-element 0))
  (load-P-and-R P1 R1 lambda-requests1 lambda-dropoffs1)
  (load-P-and-R P2 R2 lambda-requests2 lambda-dropoffs2))

(defun load-P-and-R (P R lambda-requests lambda-dropoffs)
  (loop for requests from 0
        for request-prob = (poisson requests lambda-requests)
        until (< request-prob .000001)
        do (loop for n upto 25 do 
                 (incf (aref R n) (* 10 request-prob (min requests n))))
        do (loop for dropoffs from 0 
                 for drop-prob = (poisson dropoffs lambda-dropoffs)
                 until (< drop-prob .000001)
                 do (loop for n upto 25
                          for satisfied-requests = (min requests n)
                          for new-n = (max 0 (min 20 (- (+ n dropoffs)
                                                        satisfied-requests)))
                          do (incf (aref P n new-n) (* request-prob drop-prob))))))

(defun poisson (n lambda)
  "The probability of n events according to the poisson distribution"
  (* (exp (- lambda))
     (/ (expt lambda n)
        (factorial n))))

(defun factorial (n)
  (if (= n 0)
    1
    (* n (factorial (- n 1)))))

(defun backup-action (n1 n2 a)
  (setq a (max (- n2) (min a n1)))
  (setq a (max -5 (min 5 a)))
  (+ (* -2 (abs a))
     (loop with morning-n1 = (- n1 a)
           with morning-n2 = (+ n2 a)
           for new-n1 upto 20 sum
           (loop for new-n2 upto 20 sum
                 (* (aref P1 morning-n1 new-n1)
                    (aref P2 morning-n2 new-n2)
                    (+ (aref R1 morning-n1)
                       (aref R2 morning-n2)
                       (* gamma (aref V new-n1 new-n2))))))))

(defun policy-eval ()
  (loop while (< theta
                 (loop for n1 upto 20 maximize
                       (loop for n2 upto 20
                             for old-v = (aref V n1 n2)
                             for a = (aref policy n1 n2)
                             do (setf (aref V n1 n2) (backup-action n1 n2 a))                                    
                             maximize (abs (- old-v (aref V n1 n2))))))))

(defun policy (n1 n2 &optional (epsilon .0000000001))
  (loop with best-value = -1
        with best-action
        for a from (max -5 (- n2)) upto (min 5 n1)
        for this-value = (backup-action n1 n2 a)
        do (when (> this-value (+ best-value epsilon))
             (setq best-value this-value)
             (setq best-action a))
        finally (return best-action)))

(defun show-greedy-policy ()
  (loop for n1 from 0 upto 20 do
        (format t "~%")
        (loop for n2 from 0 upto 20 do
              (format t "~3A" (policy n1 n2)))))

(defun greedify ()
  (loop with policy-improved = nil
        for n1 from 0 upto 20 do
        (loop for n2 from 0 upto 20
              for b = (aref policy n1 n2) do
              (setf (aref policy n1 n2) (policy n1 n2))
              (unless (= b (aref policy n1 n2))
                (setq policy-improved t)))
        finally (progn (show-policy) (return policy-improved))))

(defun show-policy ()
  (loop for n1 from 0 upto 20 do
        (format t "~%")
        (loop for n2 from 0 upto 20 do
              (format t "~3A" (aref policy n1 n2)))))

(defun policy-iteration ()
  (loop for count from 0
        do (policy-eval)
        do (print count)
        while (greedify)))


