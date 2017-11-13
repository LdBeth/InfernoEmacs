;; Utilities

(defun position-if-all (pred seq)
  (let ((acc nil)
        (idx 0))
    (map nil
         #'(lambda (el)
             (when (funcall pred el)
               (push idx acc))
             (incf idx))
         seq)
    (nreverse acc)))

(defun nth-all (indices ls)
  (loop for i in indices collect (nth i ls)))

(defun each-combination (fn ls)
  (let* ((size (length ls))
         (bv (make-array size :element-type 'bit :initial-element 0)))
    (flet ((succ ()
             (dotimes (i size)
               (if (zerop (sbit bv i))
                   (progn
                     (setf (sbit bv i) 1)
                     (return))
                   (setf (sbit bv i) 0)))))
      (loop repeat (expt 2 size)
         do
         (funcall fn
                  (nth-all (position-if-all #'(lambda (b) (= b 1))
                                            bv)
                           ls))
         (succ)))))



;; Main

(defvar *waiting-hands*)

(defun mahjong (tile-string)
  (setq *waiting-hands* nil)
  (map-waiting-hands (parse-hand tile-string))
  (format t "~{~A~%~}" (reverse *waiting-hands*))
  (length *waiting-hands*))

(defun parse-hand (tile-string)
  (if (/= (length tile-string) 13)
      (error "Number of tiles must be 13.")
      (let ((tiles (make-array 10 :initial-element 0)))
        (map nil
             #'(lambda (tile)
                 (incf (svref tiles (check-tile tile))))
             tile-string)
        (if (find-if #'(lambda (num) (> num 4))
                     tiles)
            (error "Each tile must not appear more than 4 times.")
            tiles))))

(defun check-tile (tile)
  (let ((d (digit-char-p tile)))
    (if (typep d '(integer 1 9))
        d
        (error "Each tile must be in [1..9]."))))

(defun map-waiting-hands (hand)
  (loop for new-tile from 1 to 9
     do
     (incf (svref hand new-tile))
     (map-complete-hands hand new-tile)
     (decf (svref hand new-tile))))

(defun map-complete-hands (hand new-tile)
  (loop for atama from 1 to 9
     do
     (when (>= (svref hand atama) 2)
       (decf (svref hand atama) 2)
       (map-with-atama hand new-tile atama)
       (incf (svref hand atama) 2))))

(defun map-with-atama (rest-hand new-tile atama)
  (each-combination
   #'(lambda (kotsu-list)
       (dolist (tile kotsu-list)
         (decf (svref rest-hand tile) 3))
       (map-with-kotsu rest-hand new-tile atama kotsu-list)
       (dolist (tile kotsu-list)
         (incf (svref rest-hand tile) 3)))
   (delete atama
           (position-if-all #'(lambda (n) (>= n 3))
                            rest-hand))))

(defun map-with-kotsu (rest-hand new-tile atama kotsu-list)
  (multiple-value-bind (syuntsu-list complete?)
      (make-syuntsu-list rest-hand)
    (when complete?
      (store-waiting-hand new-tile atama kotsu-list syuntsu-list))))

(defun make-syuntsu-list (hand)
  (let (syuntsu-list complete?)
    (labels ((rec (i)
               (if (= i 8)
                   (setq complete?
                         (if (= 0 (svref hand 8) (svref hand 9))
                             t
                             nil))
                   (let ((num (svref hand i)))
                     (if (= num 0)
                         (rec (1+ i))
                         (when (and (>= (svref hand (+ i 1)) num)
                                    (>= (svref hand (+ i 2)) num))
                           (decf (svref hand i) num)
                           (decf (svref hand (+ i 1)) num)
                           (decf (svref hand (+ i 2)) num)
                           (push (cons i num) syuntsu-list)
                           (rec (1+ i))
                           (incf (svref hand i) num)
                           (incf (svref hand (+ i 1)) num)
                           (incf (svref hand (+ i 2)) num)))))))
      (rec 0))
    (values (nreverse syuntsu-list) complete?)))



;; Formatting

(defun store-waiting-hand (new-tile atama kotsu-list syuntsu-list)
  (when (= new-tile atama)
    (pushnew-hand
     (format nil "[~A]" atama)
     (kotsu-string-list kotsu-list)
     (syuntsu-string-list syuntsu-list)))
  (let ((waiting-pos (position new-tile kotsu-list)))
    (when waiting-pos
      (pushnew-hand
       (atama-string atama)
       (kotsu-string-list-2 kotsu-list waiting-pos)
       (syuntsu-string-list syuntsu-list))))
  (dolist (waiting-pos (position-if-all
                        #'(lambda (pair)
                            (in-syuntsu new-tile (car pair)))
                        syuntsu-list))
    (pushnew-hand
     (atama-string atama)
     (kotsu-string-list kotsu-list)
     (syuntsu-string-list-2 syuntsu-list waiting-pos new-tile))))



(defun pushnew-hand (atama-string kotsu-string-list syuntsu-string-list)
  (pushnew
   (apply #'concatenate 'string
          (sort (cons atama-string
                      (nconc kotsu-string-list
                             syuntsu-string-list))
                #'string<))
   *waiting-hands* :test #'string=))

(defun atama-string (a)
  (format nil "(~A~A)" a a))

(defun kotsu-string (k)
  (format nil "(~A~A~A)" k k k))

(defun kotsu-string-list (kotsu-list)
  (loop for k in kotsu-list
     collect (format nil "(~A~A~A)" k k k)))

(defun kotsu-string-list-2 (kotsu-list waiting-pos)
  (let (ls)
    (loop
       for i upfrom 0
       and k in kotsu-list
       do (if (= i waiting-pos)
              (push (format nil "[~A~A]" k k) ls)
              (push (kotsu-string k) ls)))
    ls))

(defun syuntsu-string (s)
  (format nil "(~A~A~A)" s (+ s 1) (+ s 2)))

(defun syuntsu-string-list (syuntsu-list)
  (loop for (s . n) in syuntsu-list
     nconc (loop repeat n
              collect (syuntsu-string s))))

(defun syuntsu-string-list-2 (syuntsu-list waiting-pos new-tile)
  (let (ls)
    (loop
       for i upfrom 0
       and (s . n) in syuntsu-list
       do (if (= i waiting-pos)
              (progn
                (push (format nil "[~{~A~}]" (delete new-tile
                                                     (expand-syuntsu s)))
                      ls)
                (loop repeat (1- n)
                   do (push (syuntsu-string s) ls)))
              (loop repeat n
                 do (push (syuntsu-string s) ls))))
    ls))

(defun expand-syuntsu (s)
  (list s (+ s 1) (+ s 2)))

(defun in-syuntsu (tile s)
  (or (= tile s) (= tile (+ s 1)) (= tile (+ s 2))))
