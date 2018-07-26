;;;; This file contains the code for the program interface.

(ql:quickload :cl-ppcre)
(ql:quickload :cl-dbi)
(ql:quickload :quri)
(load "util.lisp")
(load "adb.lisp")
(load "db.lisp")
(load "gen.lisp")

(defparameter *VERSION-STRING* "v1.1.0" "The version information for the program.")
(defparameter *output-dir* nil "This is the directory in which the final report will be written and the media pulled.")

(defun print-conversations ()
  "Lists the conversations present in the database in an easy-to-read, numbered list."
  (mapc (lambda (convo) (format t "(~A)~5t~A~%" (car convo) (cdr convo))) (list-conversations)))

(defun mk-report (convo path &optional start end)
  "Fetches the messages from a conversation and pulls the media before generating the html report."
  (if (uiop:absolute-pathname-p path)
      (setf *output-dir* (absolute-path path))
      (setf *output-dir* (relative-path path)))
  (let ((msgs (list-messages convo start end))
	(convo-name (lookup convo (list-conversations))))
    (mapc #'adb-fetch-media msgs)
    (write-to-file (gen-report convo-name msgs))))

(defun main ()
  "Wraps the main cli and gracefully handles Ctrl-C interrupts."
  (handler-case (cli)
    (sb-sys:interactive-interrupt ()
      (sb-ext:quit))))

(defun cli ()
  "Interactively works with the user to generate a report."
  (let ((options '()))
    (format t "Allo Conversation Scraper (ACS) ~A~%~%" *VERSION-STRING*)
    (format t "Please connect the rooted Android device containing the Allo conversations to~%be archived, then press ENTER.~%")
    (read-line)
    (adb-pull-database)
    (db-connect)
    (format t "~%Please leave the device attached for the duration of the archiving process.~%~%")
    (format t "To begin, enter the number of the conversation that you would like to archive.~%~%")
    (print-conversations)
    (terpri)
    (push (cons :id (get-typed-input (is-bounded-integer 0 (length (list-conversations))) "Conversation ID")) options)
    (format t "~%Would you like to archive the whole conversation?~%")
    (when (equalp "N" (get-typed-input (is-yesno) "(Y/N)"))
      (format t "~%Please enter a time range in the format YYYY.MM.DD (HH:MM:SS).~%")
      (format t "Note that while providing the year, month, and day are required, the time~%components are optional.~%")
      (push (cons :start (get-typed-input (is-date) "Start time")) options)
      (push (cons :end (get-typed-input (optionally (is-date)) "End time (optional)")) options))
    (format t "~%Finally, please enter a directory in which to save the report.~%")
    (push (cons :path (get-typed-input (is-directory) "Output directory")) options)
    (format t "~%Generating archive...~%")
    (mk-report (lookup :id options) (lookup :path options) (lookup :start options) (lookup :end options))
    (format t "~%Archive generated at ~A.~%" (uiop:truenamize (lookup :path options)))
    (format t "You may now safely disconnect the Android device.~%")
    (db-free)))

(defun get-typed-input (validator prompt)
  "Takes a data type and prompt text then reads and validates input from the user. If the input doesn't validate as the type given, reprompt the user."
  (format t "~A: " prompt)
  (finish-output)
  (let ((valid (funcall validator (read-line))))
    (if valid
	(unwrap-nil valid)
	(get-typed-input validator prompt))))

(defun unwrap-nil (x)
  "Turns `wrapped-nil` into `nil`."
  (if (eq 'wrapped-nil x)
      nil
      x))

;;; Validators

(defun is-anything ()
  "No validation."
  (lambda (i) i))

(defun is-something ()
  "Ensures the string isn't empty."
  (lambda (i) (ensure-string-content i)))

(defun is-yesno ()
  "Ensures input is `y`,`Y`,`n`, or `N`."
  (lambda (i)
    (let ((safe-i (trim-whitespace i)))
      (if (or (equalp "Y" safe-i) (equalp "N" safe-i))
	  safe-i
	  nil))))

(defun is-directory ()
  "Ensures the string is a directory."
  (lambda (i)
    (when (ensure-string-content i)
      (if (pathname-name i)
	  (concatenate 'string i "/")
	  i))))

(defun is-bounded-integer (lb ub)
  "Ensures input is an integer and within a range."
  (lambda (i)
    (let ((int (handler-case (parse-integer i) (error () nil))))
      (if (and int (> int lb) (< int ub))
	  int
	  nil))))

(defun is-date ()
  "Ensures input is in `YYYY.MM.DD` + ` (HH:MM:SS)` format."
  (lambda (i)
    (let ((safe-i (trim-whitespace i))
	  (date-regex "^[0-9]{4}\.\\b([1-9]|1[0-2])\\b\.\\b([1-9]|2[0-9]|3[0-1])\\b")
	  (time-regex " \\(\\b([0-1][0-9]|2[0-3])\\b(:\\b[0-5][0-9]\\b){2}\\)$"))
      (if (or (cl-ppcre:scan (concatenate 'string date-regex "$") safe-i)
	      (cl-ppcre:scan (concatenate 'string date-regex time-regex) safe-i))
	  safe-i
	  nil))))

(defun optionally (validator)
  "Ensures an empty string or another validator."
  (lambda (i)
    (if (ensure-string-content i)
	(funcall validator i)
	'wrapped-nil)))
