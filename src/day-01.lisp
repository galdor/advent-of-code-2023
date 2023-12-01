(defpackage :aoc2023-01
  (:use :cl :aoc2023-utils)
  (:export
   #:*lines*
   #:solve-1
   #:solve-2))

(in-package :aoc2023-01)

(defvar *lines* (input-file-lines 1))

(defparameter *digit-names*
  (vector "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(defparameter *reversed-digit-names*
  (map 'vector #'reverse *digit-names*))

(defun solve-1 ()
  (let ((sum 0))
    (dolist (line *lines* sum)
      (let ((digit1 (parse-digit (find-if #'digit-char-p line)))
            (digit2 (parse-digit (find-if #'digit-char-p line :from-end t))))
        (incf sum (+ (* digit1 10) digit2))))))

(defun solve-2 ()
  (let ((sum 0))
      (dolist (line *lines* sum)
        (let ((digit1 (find-digit-value-or-name line))
              (digit2 (find-digit-value-or-name line :from-end t)))
          (incf sum (+ (* digit1 10) digit2))))))

(defun find-digit-value-or-name (string &key from-end)
  (declare (type string string))
  (do ((digit-names  (if from-end *reversed-digit-names* *digit-names*))
       (string (if from-end (reverse string) string))
       (i 0 (1+ i))
       (end (length string)))
      ((>= i end)
       nil)
    (cond
      ((digit-char-p (char string i))
       (return-from find-digit-value-or-name (parse-digit (char string i))))
      (t
       (dotimes (index 9)
         (let ((digit-name (aref digit-names index)))
           (when (and (< (length digit-name) (- end i))
                      (string= string digit-name
                               :start1 i :end1 (+ i (length digit-name))))
             (return-from find-digit-value-or-name (1+ index)))))))))

(defun parse-digit (c)
  (declare (type character c))
  (- (char-code c) #.(char-code #\0)))
