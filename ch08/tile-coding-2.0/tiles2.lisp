#| 
External documentation and recommendations on the use of this code is
available at http://www.cs.umass.edu/~rich/tiles2.html.

This is an implementation of grid-style tile codings, based originally on
the UNH CMAC code (see http://www.ece.unh.edu/robots/cmac.htm). 
Here we provide a function, "get-tiles", that maps floating and integer
variables to a list of tiles. This function is memoryless and requires no
setup...but only if hashing collisions are to be ignored.  This is a second 
version that optionally checks for hashing collisions and handles them appropriately
to simulate an infinite, accurate memory.  (Until we run out of memory
of course.)  We use open addressing with double hashing indexing.
Memory-size must be a power of 2.  

The float variables will be gridded at unit intervals, so generalization
will be by approximately 1 in each direction, and any scaling will have 
to be done externally before calling tiles.

It is recommended by the UNH folks that num-tilings be a power of 2, e.g., 16. 
|#

;The following are all globals, so change the names if you want to use them yourself.
(defParameter *max-num-floats* 20)      ; maximum number of variables used in one grid
(defConstant max-longint 2147483647)
(defConstant max-longint/4 (truncate MAX-LONGINT 4))
;The following are temporary variables used by tiles.
(defVar qstate (make-array *MAX-NUM-FLOATS* :initial-element 0))
(defVar base (make-array *MAX-NUM-FLOATS* :initial-element 0))
(defVar coordinates (make-array (+ 1 *MAX-NUM-FLOATS* *MAX-NUM-FLOATS*) :initial-element 0))

(defun get-tiles (num-tilings memory-size-or-ctable floats &optional ints)
  "returns list of num-tilings tiles corresponding to variables (floats and ints), 
   then hashing down to memory-size or checking for collisions"
  (let* ((num-floats (length floats))
         (num-coordinates (+ 1 num-floats (length ints))))
    (loop for int in ints
          for i from (+ 1 num-floats) 
          do (setf (aref coordinates i) int))
    (loop for i from 0                   ; quantize state to integers
          for float in floats do 
          (setf (aref base i) 0)
          (setf (aref qstate i) (floor (* float num-tilings))))
    (loop for j below num-tilings collect 
          (progn
            (loop for i below num-floats do
                  (setf (aref coordinates i) 
                        (- (aref qstate i)
                           (mod (- (aref qstate i) (aref base i))
                                num-tilings)))
                  (incf (aref base i) (+ 1 (* 2 i))))
            (setf (aref coordinates num-floats) j)
            (if (integerp memory-size-or-ctable) 
              (hash-UNH coordinates num-coordinates memory-size-or-ctable)
              (hash coordinates num-coordinates memory-size-or-ctable))))))

(defun load-tiles (tiles starting-element num-tilings memory-size-or-ctable floats &optional ints)
  "returns list of num-tilings tiles corresponding to variables (floats and ints), 
   hashed down to memory-size, using collisions to check for collisions"
  (let* ((num-floats (length floats))
         (num-coordinates (+ 1 num-floats (length ints))))
    (loop for int in ints
          for i from (+ 1 num-floats) 
          do (setf (aref coordinates i) int))
    (loop for i from 0                   ; quantize state to integers
          for float in floats do 
          (setf (aref base i) 0)
          (setf (aref qstate i) (floor (* float num-tilings))))
    (loop for j below num-tilings do
          (loop for i below num-floats do 
                (setf (aref coordinates i) 
                      (- (aref qstate i)
                         (mod (- (aref qstate i) (aref base i))
                              num-tilings)))
                (incf (aref base i) (+ 1 (* 2 i))))
          (setf (aref coordinates num-floats) j)
          (setf (aref tiles (+ starting-element j))
                (if (integerp memory-size-or-ctable) 
                  (hash-UNH coordinates num-coordinates memory-size-or-ctable)
                  (hash coordinates num-coordinates memory-size-or-ctable))))))

(defParameter random-table 
  (make-array 2048 :initial-contents (loop repeat 2048 collect (random 65536))))

(defun hash-unh (ints num-ints m &optional (increment 449))
  "a hashing of array of ints into below m, using random table"
  (mod (loop for i below num-ints
             for int = (aref ints i)
             sum (aref random-table (mod (+ int (* increment i)) 2048)))
       m))

(defun hash (ints num-ints ct)
  "Returns index in collision table corresponding to first part of ints (an array)"
  (incf (ct-calls ct))
  (let* ((memory-size (ct-size ct))
         (cdata (ct-data ct))
         (j (hash-UNH ints num-ints memory-size))
         (safety (ct-safety ct))
         (ccheck (ecase safety
                   ((:safe :unsafe) (hash-UNH ints num-ints MAX-LONGINT 457))
                   (:super-safe (loop for i below num-ints collect (aref ints i))))))
    (cond ((equal ccheck (aref cdata j))
           (incf (ct-clearhits ct)))
          ((minusp (aref cdata j))
           (incf (ct-clearhits ct))
           (setf (aref cdata j) ccheck))
          ((eq safety :unsafe)
           (incf (ct-collisions ct)))
          (t (loop with h2 = (+ 1 (* 2 (hash-UNH ints num-ints MAX-LONGINT/4)))
                   for i from 1
                   do (incf (ct-collisions ct))
                   do (setq j (mod (+ j h2) memory-size))
                   do (when (> i memory-size) (error "Out of memory"))
                   until (equal ccheck (aref cdata j))
                   until (when (minusp (aref cdata j))
                           (setf (aref cdata j) ccheck)))))
    j))

(defStruct (COLLISION-TABLE (:conc-name ct-)
                               (:print-function ct-print))
  (size nil)
  (data (make-array size :initial-element -1))
  (safety :safe)                        ; one of :safe, :super-safe, or :unsafe
  (calls 0)
  (clearhits 0)
  (collisions 0))

(defun make-ct (&optional (size 2048) (safety :safe))
  (unless (power-of-2? size) (error "Size of collision table (~A) must be a power of 2"))
  (make-collision-table :size size :safety safety))

(defun ct-print (ct stream print-depth)
  (format stream "#CT<~A/~A,~A,~A,~A,~A>"
          (ct-usage ct) (ct-size ct) (ct-calls ct) (ct-clearhits ct) (ct-collisions ct) (ct-safety ct)))

(defun ct-reset (ct)
  (fill (ct-data ct) -1)
  (setf (ct-calls ct) 0)
  (setf (ct-clearhits ct) 0)
  (setf (ct-collisions ct) 0))

(defun ct-stats (ct)
  (list (ct-calls ct)
        (ct-clearhits ct)
        (ct-collisions ct) 
        (ct-usage ct)))

(defun ct-usage (ct)
  (loop for d across (ct-data ct) count (not (minusp d))))

(defun power-of-2? (n)
  (= 0 (nth-value 1 (truncate (log n 2)))))

;--------------------Versions with wrapping of the floats-------------------

(defun get-tiles-wrap (num-tilings memory-size-or-ctable floats wrap-widths &optional ints)
  "returns list of num-tilings tiles corresponding to variables (floats and ints), 
   wrapping each float to its wrap-width (nil or an integer),
   then hashing down to memory-size or checking for collisions"
  (let* ((num-floats (length floats))
         (num-coordinates (+ 1 num-floats (length ints)))
         wrap-widths*num-tilings)
    (loop for int in ints
          for i from (+ 1 num-floats) 
          do (setf (aref coordinates i) int))
    (setq wrap-widths*num-tilings
          (loop for i from 0                   ; quantize state to integers
                for float in floats
                for wrap-width in wrap-widths do
                (setf (aref base i) 0)
                (setf (aref qstate i) (floor (* float num-tilings)))
                collect (and wrap-width (* wrap-width num-tilings))))
    (loop for j below num-tilings collect 
          (progn
            (loop for i below num-floats 
                  for width in wrap-widths*num-tilings do
                  (setf (aref coordinates i) 
                        (- (aref qstate i)
                           (mod (- (aref qstate i) (aref base i))
                                num-tilings)))
                  (if width (setf (aref coordinates i) (mod (aref coordinates i) width)))
                  (incf (aref base i) (+ 1 (* 2 i))))
            (setf (aref coordinates num-floats) j)
            (if (integerp memory-size-or-ctable) 
              (hash-UNH coordinates num-coordinates memory-size-or-ctable)
              (hash coordinates num-coordinates memory-size-or-ctable))))))

(defun load-tiles-wrap (tiles starting-index num-tilings memory-size-or-ctable floats 
                                 wrap-widths &optional ints)
  "Loads into array tiles, starting at starting-index, num-tilings tile indices 
     corresponding to variables (floats and ints), wrapping each float to its 
     wrap-width (nil or an integer), then hashing down to memory-size or checking for collisions"
  (let* ((num-floats (length floats))
         (num-coordinates (+ 1 num-floats (length ints)))
         wrap-widths*num-tilings)
    (loop for int in ints
          for i from (+ 1 num-floats) 
          do (setf (aref coordinates i) int))
    (setq wrap-widths*num-tilings
          (loop for i from 0                   ; quantize state to integers
                for float in floats
                for wrap-width in wrap-widths do
                (setf (aref base i) 0)
                (setf (aref qstate i) (floor (* float num-tilings)))
                collect (and wrap-width (* wrap-width num-tilings))))
    (loop for j below num-tilings do
          (loop for i below num-floats 
                for width in wrap-widths*num-tilings do
                (setf (aref coordinates i) 
                      (- (aref qstate i)
                         (mod (- (aref qstate i) (aref base i))
                              num-tilings)))
                (if width (setf (aref coordinates i) (mod (aref coordinates i) width)))
                (incf (aref base i) (+ 1 (* 2 i))))
          (setf (aref coordinates num-floats) j)
          (setf (aref tiles (+ starting-index j))
                (if (integerp memory-size-or-ctable) 
                  (hash-UNH coordinates num-coordinates memory-size-or-ctable)
                  (hash coordinates num-coordinates memory-size-or-ctable))))))

