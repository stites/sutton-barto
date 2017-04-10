;;; Code for access-control queuing problem from chapter 6.
;;; N is the number of servers, M is the number of priorities

;;; Using R-learning

(defvar N 10)
(defvar N+1)
(defvar num-states)
(defvar M 2)
;(defvar h)
(defvar p .05)
(defvar alpha .1)
(defvar beta .01)
(defvar epsilon .1)
(defvar Q)
(defvar count)
(defvar rho)
(defvar num-free-servers)               ; these two are
(defvar priority)                       ; the state variables
(defvar reward)

(defun setup ()
  (setq N+1 (+ N 1))
  (setq num-states (* M N+1))
  (setq Q (make-array (list num-states 2) :initial-element 0))
  (setq count (make-array (list num-states 2) :initial-element 0))
;  (loop for s below num-states do
;        (setf (aref Q s 0) -.1)
;        (setf (aref Q s 1) +.1))
  (setq reward (make-array M :initial-contents '(1 2 4 8)))
;  (setq h (make-array M :initial-contents '((/ 1 3) (/ 1 3) (/ 1 3)))) 
  (setq rho 0)
  (setq num-free-servers N)
  (new-priority))

(defun new-priority ()
  (setq priority (random M)))

(defun R-learning (steps)
  (loop repeat steps
        for s = (+ num-free-servers (* priority N+1)) then s-prime
        for a = (with-prob epsilon (random 2)
                  (if (> (aref Q s 0) (aref Q s 1)) 0 1))
        for r = (if (AND (= a 1) (> num-free-servers 0))
                  (aref reward priority)
                  0)
        for new-priority = (new-priority)
        for s-prime = (progn (unless (= r 0) (decf num-free-servers))
                             (loop repeat (- N num-free-servers)
                                   do (when (with-probability p)
                                        (incf num-free-servers)))
                             (+ num-free-servers (* new-priority N+1)))
;        do (print (list s a r s-prime rho (max (aref Q s-prime 0) (aref Q s-prime 1))))
        do (incf (aref Q s a) (* alpha (+ r (- rho)
                                          (max (aref Q s-prime 0) 
                                               (aref Q s-prime 1))
                                          (- (aref Q s a)))))
        do (incf (aref count s a))
        do (when (= (aref Q s a) (max (aref Q s 0) (aref Q s 1)))
             (incf rho (* beta (+ r (- rho)
                                  (max (aref Q s-prime 0) (aref Q s-prime 1))
                                  (- (max (aref Q s 0) (aref Q s 1)))))))
        do (setq priority new-priority)))

(defun policy ()
  (loop for pri below M do
        (format t "~%")
        (loop for free upto N
              for s = (+ free (* pri N+1)) 
              do (format t (if (> (aref Q s 0) (aref Q s 1)) " 0" " 1"))))
  (values))

(defun num ()
  (loop for pri below M do
        (format t "~%")
        (loop for free upto N
              for s = (+ free (* pri N+1)) 
              do (format t "~A/~A " (aref count s 0) (aref count s 1))))
  (values))

(defun Q ()
  (loop for pri below M do
        (format t "~%")
        (loop for free upto N
              for s = (+ free (* pri N+1)) 
              do (format t "~6,3F/~6,3F " (aref Q s 0) (aref Q s 1))))
  (values))

(defun gr ()
  (graph (cons (list '(1 0) (list N+1 0))
               (loop for pri below M  
                     collect (loop for free upto N
                                   collect (aref Q (+ free (* pri N+1)) 0))
                     collect (loop for free upto N
                                   collect (aref Q (+ free (* pri N+1)) 1))))))

(defun grV* ()
  (graph (cons (list '(1 0) (list N+1 0))
               (loop for pri below M  
                     collect (loop for free upto N
                                   collect (max (aref Q (+ free (* pri N+1)) 0)
                                                (aref Q (+ free (* pri N+1)) 1)))))))

