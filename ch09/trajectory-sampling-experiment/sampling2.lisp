(defvar n 0)
(defvar k 0)
(defvar successor)
(defvar R)
(defvar Q)
(defvar gamma .9)
(defvar alpha .1)
(defvar epsilon .1)
(defvar randomness)
(defvar max-num-tasks 2000)
(defvar policy)
(defvar V)

(defun setup (n-arg k-arg)
  (setq n n-arg)
  (setq k k-arg)
  (setq successor (make-array (list n 2 k)))
  (setq R (make-array (list n (+ k 1) 2)))  
  (setq Q (make-array (list n 2)))
  (setq policy (make-array n))
  (setq V (make-array n))
  (setq randomness (make-array max-num-tasks))
  (standardize-random-state)
  (advance-random-state 0)
  (loop for task below max-num-tasks do
        (loop repeat 17 do (random 2))
        (setf (aref randomness task) (make-random-state))))

(defun init (task-num)
  (setq *random-state* (make-random-state (aref randomness task-num)))
  (loop for s below n do
        (loop for a below 2 do
              (setf (aref Q s a) 0.0)
              (setf (aref R s k a) (random-normal))
              (loop for sp in (random-k-of-n k n)
                    for i below k
                    do (setf (aref successor s a i) sp)
                    do (setf (aref R s i a) (random-normal))))))

(defun random-k-of-n (k n)
  (loop for i = (random n)
        unless (member i result) collect i into result
        until (= k (length result))
        finally (return result)))

(defun next-state (s a)
  (with-prob gamma
    (aref successor s a (random k))
    n))

(defun full-backup (s a)
  (+ (* (- 1 gamma) (aref R s k a))
     (* gamma (/ k)
        (loop for i below k
              for sp = (aref successor s a i)
              sum (aref R s i a)
              sum (* gamma (loop for ap below 2 
                                 maximize (aref Q sp ap)))))))

(defun runs-sweeps (n-arg k-arg num-runs num-sweeps sweeps-per-measurement)
  (unless (and (= n n-arg) (= k k-arg)) (setup n-arg k-arg))
  (loop with backups-per-measurement = (truncate (* sweeps-per-measurement 2 n))
        with backups-per-sweep = (* n 2)
        with num-backups = (* num-sweeps backups-per-sweep)
        with num-measurements = (truncate num-backups backups-per-measurement)
        with perf = (make-array num-measurements :initial-element 0.0)
        for run below num-runs do
        (init run)
        (format t "~A " run)
        (loop with backups = 0
              repeat num-sweeps do
              (loop for s below n do
                    (loop for a below 2 do
                          (when (= 0 (mod backups backups-per-measurement))
                            (incf (aref perf (/ backups backups-per-measurement))
                                  (measure-performance)))
                          (setf (aref Q s a) (full-backup s a))
                          (incf backups))))
        finally (record n k num-runs num-sweeps sweeps-per-measurement gamma 1 nil
                        (loop for i below num-measurements
                              collect (/ (aref perf i) num-runs)))))

(defun runs-trajectories (n-arg k-arg num-runs num-sweeps sweeps-per-measurement)
  (unless (and (= n n-arg) (= k k-arg)) (setup n-arg k-arg))
  (loop with backups-per-measurement = (truncate (* sweeps-per-measurement 2 n))
        with backups-per-sweep = (* n 2)
        with num-backups = (* num-sweeps backups-per-sweep)
        with num-measurements = (truncate num-backups backups-per-measurement)
        with perf = (make-array num-measurements :initial-element 0.0)
        for run below num-runs do
        (init run)
        (format t "~A " run)
        (loop named run with backups = 0 do
              (loop for state = 0 then next-state
                    for action = (with-prob epsilon
                                   (random 2)
                                   (if (>= (aref Q state 0) (aref Q state 1)) 0 1))
                    for next-state = (next-state state action) do
                    (when (= 0 (mod backups backups-per-measurement))
                      (incf (aref perf (/ backups backups-per-measurement))
                            (measure-performance)))
                    (setf (aref Q state action) (full-backup state action))
                    (incf backups)
                    (when (= backups num-backups) (return-from run))
                    until (= next-state n)))
        finally (record n k num-runs num-sweeps sweeps-per-measurement gamma 1 epsilon
                        (loop for i below num-measurements
                              collect (/ (aref perf i) num-runs)))))

(defun measure-performance ()
  (loop for s below n do
        (setf (aref V s) 0.0)
        (setf (aref policy s) 
              (if (>= (aref Q s 0) (aref Q s 1)) 
                0 1)))
  (loop for delta = (loop for s below n
                          for old-V = (aref V s)
                          do (setf (aref V s) (full-backup s (aref policy s)))
                          sum (abs (- old-V (aref V s))))
        until (< delta .001))
  (aref V 0))

(defun both (n-arg k-arg runs-arg sweeps-arg measure-arg)
    (runs-sweeps n-arg k-arg runs-arg sweeps-arg measure-arg)
    (runs-trajectories n-arg k-arg runs-arg sweeps-arg measure-arg)
    (graph-data :n n-arg :k k-arg :runs runs-arg :sweeps sweeps-arg :sweeps-per-measurement measure-arg))

(defun big-exp ()
  (both 10 1 200 10 1)
  (both 10 3 200 10 1)
  (both 100 1 200 10 .5)
  (both 100 3 200 10 .5)
  (both 100 10 200 10 .5)
  (both 1000 1 200 10 .2)
  (both 1000 3 200 10 .2)
  (both 1000 10 200 10 .2)
  (both 1000 20 200 10 .2)
  (both 10000 1 100 10 .1)
  (both 10000 3 200 10 .1)
  (both 10000 10 200 10 .1)
  (both 10000 20 200 10 .1)
  (both 10000 50 200 10 .1))

