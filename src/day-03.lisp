(defpackage :aoc2023-03
  (:use :cl :aoc2023-utils)
  (:export
   #:*data*
   #:solve-1
   #:solve-2))

(in-package :aoc2023-03)

(defvar *data* (input-file-data 3))

(defmacro do-parts ((part-start part-end part data &optional result)
                    &body body)
  (let ((data-var (gensym "DATA-"))
        (end (gensym "END-"))
        (i (gensym "I-")))
    `(do* ((,data-var ,data)
           (,end (length ,data-var))
           (,i 0))
          ((>= ,i ,end)
           ,result)
       (let ((,part-start (position-if #'digit-char-p ,data-var :start ,i)))
         (cond
           (,part-start
            (let* ((,part-end (or (position-if-not #'digit-char-p ,data-var
                                                   :start part-start)
                                  ,end))
                   (,part (parse-integer ,data-var :start part-start
                                                   :end part-end)))
              ,@body
              (setf ,i (1+ ,part-end))))
           (t
            (setf ,i ,end)))))))

;; 543'867
(defun solve-1 ()
  (let ((width (grid-width *data*))
        (sum 0))
    (do-parts (part-start part-end part *data* sum)
      (let ((symbols (grid-adjacent-symbols *data* width part-start part-end)))
        (when symbols
          (incf sum part))))))

;; 79'613'331
(defun solve-2 ()
  (let ((width (grid-width *data*))
        (gears (make-hash-table))
        (sum 0))
    (do-parts (part-start part-end part *data* sum)
      (dolist (symbol (grid-adjacent-symbols *data* width part-start part-end))
        (destructuring-bind (i . c) symbol
          (when (char= c #\*)
            (let ((gear (gethash i gears)))
              (if gear
                  (let ((ratio (* (cdr gear) part)))
                    (incf sum ratio))
                  (setf (gethash i gears) (cons i part))))))))))

(defun grid-width (data)
  (declare (type string data))
  (1+ (position #\Newline data)))

(defun grid-adjacent-symbols (data width start end)
  (declare (type string data)
           (type (integer 0) width start end))
  (let ((indices nil))
    (flet ((add (i)
             (when (< -1 i (length data))
               (let ((c (aref data i)))
                 (when (char-symbolp c)
                   (push (cons i c) indices))))))
      (add (1- start))
      (add end)
      (do ((i (1- start) (1+ i)))
          ((> i end))
        (add (- i width))
        (add (+ i width))))
    indices))

(defun char-symbolp (c)
  (declare (type character c))
  (not (or (digit-char-p c)
           (char= c #\Newline)
           (char= c #\.))))
