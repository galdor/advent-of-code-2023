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
    (count-steps '("AAA") instructions nodes)))

;; 9'606'140'307'013
(defun solve-2 ()
  (multiple-value-bind (instructions nodes)
      (parse-input *lines*)
    (count-steps (start-nodes nodes) instructions nodes)))

(defun count-steps (start-nodes instructions nodes)
  (declare (type string instructions)
           (type hash-table nodes))
  (let ((periods (mapcar (lambda (node)
                           (period node instructions nodes))
                         start-nodes)))
    (apply #'lcm periods)))

(defun period (start-node instructions nodes)
  (declare (type string start-node instructions)
           (type hash-table nodes))
  (do ((n 0 (1+ n))
       (node start-node))
      (nil)
    (let ((instruction (aref instructions (mod n (length instructions)))))
      (when (end-node-p node)
        (return-from period n))
      (setf node (next-step node instruction nodes)))))

(defun next-step (node instruction nodes)
  (declare (type string node)
           (type character instruction)
           (type hash-table nodes))
  (let ((entry (gethash node nodes)))
    (case instruction
      (#\L (car entry))
      (#\R (cdr entry)))))

(defun start-nodes (nodes)
  (declare (type hash-table nodes))
  (let ((start-nodes nil))
    (maphash (lambda (node destinations)
               (declare (ignore destinations))
               (when (char= (char node 2) #\A)
                 (push node start-nodes)))
             nodes)
    start-nodes))

(defun end-node-p (node)
  (declare (type string node))
  (char= (char node 2) #\Z))

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
