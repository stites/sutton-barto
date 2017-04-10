;;; Monte Carlo and DP solution of simple blackjack.

;;; The state is (dc,pc,ace01), i.e., (dealer-card, player-count, usable-ace?),
;;; in the ranges ([12-21],[12-21],[0-1]).  

;;; The actions are hit or stick, t or nil

(defvar V)
(defvar policy)
(defvar N)                              ; Number of returns seen for this state
(defvar dc)                             ; count of dealer's showing card
(defvar pc)                             ; total count of player's hand
(defvar ace)                            ; does play have a usable ace?
(defvar episode)

(defun card ()
  (min 10 (+ 1 (random 13))))

(defun setup ()
  (setq V (make-array '(11 22 2) :initial-element 0.0))
  (setq N (make-array '(11 22 2) :initial-element 0))
  (setq policy (make-array '(11 22 2) :initial-element 1))
  (loop for dc from 1 to 10 do
        (loop for pc from 20 to 21 do
              (loop for ace from 0 to 1 do
                    (setf (aref policy dc pc ace) 0)))))

(defun episode ()
  (let (dc-hidden pcard1 pcard2 outcome)
    (setq episode nil)
    (setq dc-hidden (card))
    (setq dc (card))
    (setq pcard1 (card))
    (setq pcard2 (card))
    (setq ace (OR (= 1 pcard1) (= 1 pcard2)))
    (setq pc (+ pcard1 pcard2))
    (if ace (incf pc 10))
    (unless (= pc 21)                   ; natural blackjack ends all
      (loop do (push (list dc pc ace) episode)
            while (= 1 (aref policy dc pc (if ace 1 0)))
            do (draw-card)
            until (bust?)))
    (setq outcome (outcome dc dc-hidden))
    (learn episode outcome)
    (cons outcome episode)))

(defun learn (episode outcome)
  (loop for (dc pc ace-boolean) in episode
        for ace = (if ace-boolean 1 0) do
        (when (> pc 11)
          (incf (aref N dc pc ace))
          (incf (aref V dc pc ace) (/ (- outcome (aref V dc pc ace))
                                      (aref N dc pc ace))))))  

(defun outcome (dc dc-hidden)
  (let (dcount dace dnatural pnatural)
    (setq dace (OR (= 1 dc) (= 1 dc-hidden)))
    (setq dcount (+ dc dc-hidden))
    (if dace (incf dcount 10))
    (setq dnatural (= dcount 21))
    (setq pnatural (not episode))
    (cond
     ((AND pnatural dnatural) 0)
     (pnatural 1)
     (dnatural -1)
     ((bust?) -1)
     (t (loop while (< dcount 17)
              for card = (card) do
              (incf dcount card)
              (when (AND (not dace) (= card 1))
                (incf dcount 10)
                (setf dace t))
              (when (AND dace (> dcount 21))
                (decf dcount 10)
                (setq dace nil))
              finally (return (cond ((> dcount 21) 1)
                                    ((> dcount pc) -1)
                                    ((= dcount pc) 0)
                                    (t 1))))))))

(defun draw-card ()
  (let (card)
    (setq card (card))
    (incf pc card)
    (when (AND (not ace) (= card 1))
      (incf pc 10)
      (setf ace t))
    (when (AND ace (> pc 21))
      (decf pc 10)
      (setq ace nil))))

(defun bust? ()
  (> pc 21))

(defvar w)
(defvar array (make-array '(10 10)))
(defun gr (source ace &optional (arr array))
  (loop with ace = (if ace 1 0)
        for i below 10 do
        (loop for j below 10 do
              (setf (aref arr i j) (aref source (+ i 1) (+ j 12) ace))))
  (g::graph-surface w arr))

(defun experiment ()
  (setup)
  (loop for count below 500
        for ar0 = (make-array '(10 10))
        for ar1 = (make-array '(10 10))
        do
        (print count)
        (gr V nil ar0)
        (gr V t ar1)
        collect ar0 
        collect ar1
        do (loop repeat 1000 do (episode))))

