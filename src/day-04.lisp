(defpackage :aoc2023-04
  (:use :cl :aoc2023-utils)
  (:export
   #:*lines*
   #:solve-1
   #:solve-2))

(in-package :aoc2023-04)

(defvar *lines* (input-file-lines 4))

;; 18'619
(defun solve-1 ()
  (let ((sum 0))
    (dolist (line *lines* sum)
      (let* ((card (parse-line line))
             (matching-numbers (intersection (second card) (third card))))
        (when matching-numbers
          (incf sum (expt 2 (1- (length matching-numbers)))))))))

;; 8'063'216
(defun solve-2 ()
  (let ((cards (map 'vector 'parse-line *lines*))
        (sum 0))
    (dotimes (i (length cards) sum)
      (let* ((card (aref cards i))
             (matching-numbers (intersection (second card) (third card))))
        (dotimes (j (length matching-numbers))
          (let ((card2 (aref cards (+ i j 1))))
            (incf (fourth card2) (fourth card))))
        (incf sum (fourth card))))))

(defun parse-line (line)
  (declare (type string line))
  (flet ((number-position (&key (start 0) end)
           (position-if #'digit-char-p line :start start :end end)))
    (let* ((start (number-position))
           (colon (position #\: line :start (1+ start)))
           (card-number (parse-integer line :start start :end colon))
           (vbar (position #\| line :start (1+ colon))))
      (list card-number
            (parse-numbers line :start (number-position :start (+ colon 2))
                                :end (1- vbar))
            (parse-numbers line :start (number-position :start (+ vbar 2)))
            1))))

(defun parse-numbers (string &key (start 0) end)
  (declare (type string string)
           (type (integer 0) start)
           (type (or (integer 0) null) end))
  (do ((end (or end (length string)))
       (i start)
       (numbers nil))
      ((or (null i) (>= i end))
       (nreverse numbers))
    (let* ((space (position #\Space string :start i :end end))
           (number-end (or space end))
           (number (parse-integer string :start i :end number-end)))
      (push number numbers)
      (when (< i end)
        (setf i (position-if #'digit-char-p string
                             :start number-end :end end))))))
