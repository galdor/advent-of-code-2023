(defpackage :aoc2023-05
  (:use :cl :aoc2023-utils)
  (:export
   #:*lines*
   #:solve-1
   #:solve-2))

(in-package :aoc2023-05)

(defvar *lines* (input-file-lines 5))

(deftype range ()
  '(cons integer integer))

;; 324'724'204
(defun solve-1 ()
  (multiple-value-bind (seeds tables)
      (parse-input *lines*)
    (minimal-location (mapcar (lambda (seed)
                                (cons seed seed))
                              seeds)
                      tables)))

;; 104'070'862
(defun solve-2 ()
  (multiple-value-bind (seed-pairs tables)
      (parse-input *lines*)
    (do ((pairs seed-pairs (cddr pairs))
         (ranges nil))
        ((null pairs)
         (minimal-location ranges tables))
      (let ((start (car pairs))
            (length (cadr pairs)))
        (push (cons start (+ start length)) ranges)))))

(defun parse-input (lines)
  (declare (type list lines))
  (let (seeds tables)
    (let* ((line (car lines))
           (colon (position #\: line)))
      (setf seeds (mapcar #'parse-integer
                          (core:split-string line " " :start (+ colon 2)))))
    (let ((table nil))
      (dolist (line (cdr lines))
        (cond
          ((string= line "")
           nil)
          ((not (digit-char-p (char line 0)))
           (when table
             (push (finalize-table table) tables)
             (setf table nil)))
          (t
           (destructuring-bind (destination source range-length)
               (mapcar #'parse-integer (core:split-string line " "))
             ;; Directly pre-compute the input range and the offset to apply
             ;; during conversion.
             (push (cons (cons source (+ source range-length -1))
                         (- destination source))
                   table)))))
      (push (finalize-table table) tables))
    (values seeds (nreverse tables))))

(defun finalize-table (table)
  (declare (type list table))
  (let ((final-table nil)
        (last 0))
    (dolist (entry (sort table #'< :key #'caar) (nreverse final-table))
      (let ((range (car entry)))
        ;; Generate ranges to cover all possible values
        (when (> (car range) last)
          (push (cons (cons last (1- (car range))) 0) final-table))
        (setf last (1+ (cdr range))))
      (push entry final-table))))

(defun range-intersection (range1 range2)
  (declare (type range range1 range2))
  (when (and (>= (cdr range1) (car range2))
             (<= (car range1) (cdr range2)))
    (cons (max (car range1) (car range2))
          (min (cdr range1) (cdr range2)))))

(defun convert-range (range table)
  (declare (type range range)
           (type list table))
  (let ((ranges2 nil))
    (dolist (entry table)
      (destructuring-bind (entry-range . offset) entry
        (let ((intersection (range-intersection range entry-range)))
          (when intersection
            (push (cons (+ (car intersection) offset)
                        (+ (cdr intersection) offset))
                  ranges2)))))
    ;; It is possible for values to be higher than the highest range, but in
    ;; this case the offset is zero.
    (or ranges2 (list range))))

(defun convert-ranges (ranges table)
  (declare (type list ranges table))
  (let ((ranges2 nil))
    (dolist (range ranges)
      (push (convert-range range table) ranges2))
    (reduce #'append ranges2)))

(defun minimal-location (ranges tables)
  (declare (type list ranges tables))
  (let ((ranges ranges))
    (dolist (table tables)
      (setf ranges (convert-ranges ranges table)))
    (apply #'min (mapcar #'car ranges))))
