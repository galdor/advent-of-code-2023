(defpackage :aoc2023-utils
  (:use :cl)
  (:export
   #:*year*
   #:*firefox-profile-directory*
   #:*http-user-agent*
   #:firefox-cookie
   #:default-firefox-profile
   #:download-input-file
   #:input-file-path
   #:input-file-data
   #:input-file-lines))

(in-package :aoc2023-utils)

(defparameter *year* 2023
  "The year of the Advent of Code event.")

(defparameter *firefox-profile-directory*
  (merge-pathnames
   (make-pathname :directory '(:relative ".mozilla" "firefox"))
   (user-homedir-pathname))
  "The path of the directory containing Firefox profiles.")

(defparameter *http-user-agent*
  (format nil "https://github.com/galdor/advent-of-code-~D" *year*)
  "The user agent used for requests sent to the Advent of Code website.")

(defmacro with-temporary-file-copy ((copy-path path) &body body)
  "Evaluate BODY with COPY-PATH bound to the path of a temporary file created as
a copy of the file at PATH. The temporary file is always deleted after BODY
has been evaluated."
  (let ((input (gensym "INPUT-"))
        (output (gensym "OUTPUT-"))
        (data (gensym "DATA-")))
    `(let ((,copy-path (make-pathname :name (pathname-name ,path) :type "tmp"
                                      :defaults ,path)))
       (unwind-protect
            (progn
              (with-open-file (,input ,path :element-type '(unsigned-byte 8))
                (with-open-file
                    (,output ,copy-path :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create
                                        :element-type '(unsigned-byte 8))
                  (let ((,data (make-array
                                (file-length ,input)
                                :element-type (stream-element-type ,input))))
                    (read-sequence ,data ,input)
                    (write-sequence ,data ,output))))
              ,@body)
         (delete-file ,copy-path)))))

(defun firefox-cookie ()
  "Return the value of the cookie for the Advent of Code website stored in the
default Firefox profile.

Because Firefox stupidly locks its SQLite databases, we have to copy the
entire database file before reading it. We cannot even use the sqlite .dump
command, because the file is locked. Fingers crossed."
  (let* ((profile (default-firefox-profile))
         (cookie-db-path
           (merge-pathnames (make-pathname :directory (list :relative profile)
                                           :name "cookies" :type "sqlite")
                            *firefox-profile-directory*)))
    (with-temporary-file-copy (tmp-path cookie-db-path)
      (let* ((domain ".adventofcode.com")
             (query (format nil "SELECT value FROM moz_cookies WHERE host='~A'"
                            domain))
             (command (list "sqlite3" "-readonly" "-batch" "-list"
                            (namestring tmp-path) query)))
        (multiple-value-bind (output error-output status)
            (uiop:run-program command :force-shell t
                                      :output :string
                                      :error-output t)
          (declare (ignore error-output status))
          ;; The first line is the header line
          (let ((second-line-start (position #\Newline output)))
            (unless (and second-line-start
                         (< second-line-start (1- (length output))))
              (error "cannot parse sqlite output ~S" output))
            (let ((end (position #\Newline output
                                 :start (1+ second-line-start))))
              (subseq output (1+ second-line-start) end))))))))

(defun default-firefox-profile ()
  "Return the name of the default Firefox profile."
  (let ((profiles-ini-path
          (merge-pathnames (make-pathname :name "profiles" :type "ini")
                           *firefox-profile-directory*))
        (prefix "Default="))
    (handler-case
        (with-open-file (file profiles-ini-path :external-format :utf-8)
          (loop
            (let ((line (read-line file)))
              (when (and (> (length line) (length prefix))
                         (string= line prefix :end1 (length prefix)))
                (return-from default-firefox-profile
                  (subseq line (length prefix)))))))
      (end-of-file (c)
        (declare (ignore c))
        (error "default profile name not found in ~A" profiles-ini-path)))))

(defun download-input-file (day)
  "Download an input file and store it in the input directory. Return the
absolute path of the file."
  (let* ((uri (uri:make-uri :scheme "https"
                            :host "adventofcode.com"
                            :path (format nil "/~D/day/~D/input" *year* day)))
         (cookie (firefox-cookie))
         (header
           (list (cons "User-Agent" *http-user-agent*)
                 (cons "Cookie" (concatenate 'string "session=" cookie))))
         (response (http:send-request :get uri :header header)))
    (unless (<= 200 (http:response-status response) 299)
      (error "request failed with status ~D: ~A"
             (http:response-status response)
             (text:decode-string
              (http:response-body response))))
    (let ((path (input-file-path day)))
      (with-open-file (file path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
        (write-sequence (http:response-body response) file))
      path)))

(defun input-file-path (day)
  "Return the absolute path of an input file in the repository."
  (let ((system (format nil "aoc~D" *year*))
        (subpath (make-pathname :directory '(:relative "data")
                                :name (format nil "day-~2,'0D" day)
                                :type "txt")))
    (asdf:system-relative-pathname system subpath)))

(defun input-file-data (day)
  "Return the content of an input file as a string, downloading the file if it
does not exist locally."
  (let ((path (input-file-path day)))
    (unless (probe-file path)
      (download-input-file day))
    (system:read-file path :external-format text:*default-encoding*)))

(defun input-file-lines (day)
  "Return the content of an input file as a list of lines, downloading the file
if it does not exist locally."
  (do* ((data (input-file-data day))
        (start 0)
        (end (length data))
        (lines nil))
       ((>= start end)
        (nreverse lines))
    (let ((eol (or (position #\Newline data :start start :end end) end)))
      (push (subseq data start eol) lines)
      (setf start (1+ eol)))))
