(defpackage :aoc2023-08
  (:use :cl :aoc2023-utils)
  (:export
   #:*lines*
   #:solve-1
   #:solve-2))

(in-package :aoc2023-08)

(defvar *lines* (input-file-lines 8))

;; 19'241
(defun solve-1 ()
  (multiple-value-bind (instructions nodes)
      (parse-input *lines*)
    (count-steps instructions nodes)))

(defun solve-2 ()
  ;; TODO
  nil)

(defun count-steps (instructions nodes)
  (labels ((walk (current-node nb-steps)
           (cond
             ((string= current-node "ZZZ")
              nb-steps)
             (t
              (let ((node (gethash current-node nodes)))
                (case (aref instructions (mod nb-steps (length instructions)))
                  (#\L
                   (walk (car node) (1+ nb-steps)))
                  (#\R
                   (walk (cdr node) (1+ nb-steps)))))))))
    (walk "AAA" 0)))

(defun parse-input (lines)
  (declare (type list lines))
  (values (car lines)
          (parse-nodes (cddr lines))))

(defun parse-nodes (lines)
  (declare (type list lines))
  (let ((nodes (make-hash-table :test #'equal)))
    (dolist (line lines nodes)
      (setf (gethash (subseq line 0 3) nodes)
            (cons (subseq line 7 10)
                  (subseq line 12 15))))))
