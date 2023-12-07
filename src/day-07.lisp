(defpackage :aoc2023-07
  (:use :cl :aoc2023-utils)
  (:export
   #:*lines*
   #:solve-1
   #:solve-2))

(in-package :aoc2023-07)

(defvar *lines* (input-file-lines 7))

(defparameter *jokers* nil)

(deftype card ()
  'character)

(deftype hand ()
  '(cons (simple-array card (5)) integer))

(defun solve-1 ()
  (let* ((*jokers* nil)
         (hands (parse-hands *lines*))
         (ranked-hands (rank-hands hands))
         (sum 0))
    (dolist (hand ranked-hands sum)
      (let ((rank (cdr hand))
            (bid (cdar hand)))
        (incf sum (* rank bid))))))

(defun solve-2 ()
  (let* ((*jokers* t)
         (hands (parse-hands *lines*))
         (ranked-hands (rank-hands hands))
         (sum 0))
    (dolist (hand ranked-hands sum)
      (let ((rank (cdr hand))
            (bid (cdar hand)))
        (incf sum (* rank bid))))))

(defun parse-hands (lines)
  (declare (type list lines))
  (let ((hands nil))
    (dolist (line lines (nreverse hands))
      (let ((space (position #\Space line)))
        (push (cons (subseq line 0 space)
                    (parse-integer line :start (1+ space)))
              hands)))))

(defmacro do-hand-cards ((card hand &optional result) &body body)
  (let ((cards (gensym "CARDS-"))
        (i (gensym "I-")))
    `(let ((,cards (car ,hand)))
       (dotimes (,i (length ,cards) ,result)
         (let ((,card (aref ,cards ,i)))
           ,@body)))))

(defun rank-hands (hands)
  (declare (type list hands))
  (do ((hands (sort hands 'hand-rank<) (cdr hands))
       (i 1 (1+ i))
       (pairs nil))
      ((null hands)
       pairs)
   (push (cons (car hands) i) pairs)))

(defun hand-rank< (hand1 hand2)
  (declare (type hand hand1 hand2))
  (let ((data1 (analyze-hand hand1))
        (data2 (analyze-hand hand2)))
    (cond
      ((< (cdr data1) (cdr data2))
       t)
      ((> (cdr data1) (cdr data2))
       nil)
      (t
       (dotimes (i 5)
         (let ((c1 (card-value (aref (car hand1) i)))
               (c2 (card-value (aref (car hand2) i))))
           (cond
             ((< c1 c2) (return t))
             ((> c1 c2) (return nil)))))))))

(defun analyze-hand (hand)
  (declare (type hand hand))
  (let ((counts nil))
    (do-hand-cards (card hand)
      (let ((pair (assoc card counts :test #'char=)))
        (if pair
            (incf (cdr pair))
            (push (cons card 1) counts))))
    (setf counts (sort counts #'> :key #'cdr))
    (when *jokers*
      (let ((joker-pair (assoc #\J counts)))
        (when joker-pair
          (unless (null (cdr counts))
            (setf counts (delete #\J counts :key #'car))
            (when counts
              (incf (cdar counts) (cdr joker-pair)))))))
    (cond
      ((= (cdar counts) 5)
       (cons 'five-of-a-kind 7))
      ((= (cdar counts) 4)
       (cons 'four-of-a-kind 6))
      ((and (= (cdar counts) 3)
            (= (cdadr counts) 2))
       (cons 'full-house 5))
      ((= (cdar counts) 3)
       (cons 'three-of-a-kind 4))
      ((and (= (cdar counts) 2)
            (= (cdadr counts) 2))
       (cons 'two-pairs 3))
      ((and (= (cdar counts) 2))
       (cons 'one-pair 2))
      (t
       (cons 'high-card 1)))))

(defun card-value (card)
  (declare (type card card))
  (ecase card
    (#\2  1)
    (#\3  2)
    (#\4  3)
    (#\5  4)
    (#\6  5)
    (#\7  6)
    (#\8  7)
    (#\9  8)
    (#\T  9)
    (#\J (if *jokers* 0 10))
    (#\Q 11)
    (#\K 12)
    (#\A 13)))
