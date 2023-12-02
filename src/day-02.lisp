(defpackage :aoc2023-02
  (:use :cl :aoc2023-utils)
  (:export
   #:*lines*
   #:solve-1
   #:solve-2))

(in-package :aoc2023-02)

(defvar *lines* (input-file-lines 2))

(deftype cube-set ()
  '(array integer (3)))

(defun make-cube-set ()
  (make-array 3 :element-type 'integer))

(defun solve-1 ()
  (let ((games (mapcar 'parse-game *lines*))
        (bag #(12 13 14))
        (sum 0))
    (dolist (game games sum)
      (when (bag-contains-sets bag (cdr game))
        (incf sum (car game))))))

(defun solve-2 ()
  (let ((games (mapcar 'parse-game *lines*))
        (sum 0))
    (dolist (game games sum)
      (incf sum (power (minimal-bag (cdr game)))))))

(defun parse-game (line)
  (declare (type string line))
  (let* ((colon (position #\: line))
         (game-id (parse-integer line :start 5 :end colon))
         (sets nil))
    (dolist (set-string (core:split-string line "; " :start (+ colon 2)))
      (let ((set (make-cube-set)))
        (dolist (part (core:split-string set-string ", "))
          (let* ((space (position #\Space part))
                 (color-index (core:string-case (subseq part (1+ space))
                                ("red"   0)
                                ("green" 1)
                                ("blue"  2)))
                 (count (parse-integer part :end space)))
            (setf (aref set color-index) count)))
        (push set sets)))
    (cons game-id (nreverse sets))))

(defun bag-contains-set (bag set)
  (declare (type cube-set bag set))
  (and (<= (aref set 0) (aref bag 0))
       (<= (aref set 1) (aref bag 1))
       (<= (aref set 2) (aref bag 2))))

(defun bag-contains-sets (bag sets)
  (declare (type cube-set bag)
           (type list sets))
  (every (lambda (set) (bag-contains-set bag set)) sets))

(defun minimal-bag (sets)
  (declare (type list sets))
  (let ((bag (make-cube-set)))
    (dolist (set sets bag)
      (setf (aref bag 0) (max (aref bag 0) (aref set 0))
            (aref bag 1) (max (aref bag 1) (aref set 1))
            (aref bag 2) (max (aref bag 2) (aref set 2))))))

(defun power (set)
  (declare (type cube-set set))
  (* (aref set 0) (aref set 1) (aref set 2)))
