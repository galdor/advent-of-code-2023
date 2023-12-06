(defpackage :aoc2023-06
  (:use :cl :aoc2023-utils)
  (:export
   #:*lines*
   #:solve-1
   #:solve-2))

(in-package :aoc2023-06)

(defvar *lines* (input-file-lines 6))

;; 1'312'850
(defun solve-1 ()
  (let ((races (parse-races *lines*))
        (product 1))
    (dolist (race races product)
      (destructuring-bind (race-duration . record-distance) race
        (let ((nb-wins 0))
          (dotimes (hold-time race-duration)
            (when (> (race-distance hold-time race-duration) record-distance)
              (incf nb-wins)))
          (setf product (* product nb-wins)))))))

;; 36'749'103
(defun solve-2 ()
  (let ((race (parse-single-race *lines*)))
    (destructuring-bind (race-duration . record-distance) race
      (let ((nb-wins 0))
        (dotimes (hold-time race-duration nb-wins)
          (when (> (race-distance hold-time race-duration) record-distance)
            (incf nb-wins)))))))

(defun race-distance (hold-time race-duration)
  (declare (type integer hold-time race-duration))
  (* (- race-duration hold-time) hold-time))

(defun parse-races (lines)
  (declare (type list lines))
  (flet ((parse-line (line)
           (declare (type string line))
           (do ((i (1+ (position #\: line)))
                (end (length line))
                (numbers nil))
               ((>= i end)
                (nreverse numbers))
             (let* ((number-start
                      (position-if #'digit-char-p line :start i))
                    (number-end
                      (or (position #\Space line :start (1+ number-start))
                          end)))
               (push (parse-integer line :start number-start :end number-end)
                     numbers)
               (setf i number-end)))))
    (mapcar #'cons
            (parse-line (first lines))
            (parse-line (second lines)))))

(defun parse-single-race (lines)
  (declare (type list lines))
  (flet ((parse-line (line)
           (declare (type string line))
           (let* ((start (1+ (position #\: line)))
                  (string (delete #\Space (subseq line start))))
             (parse-integer string))))
    (cons (parse-line (first lines))
          (parse-line (second lines)))))
