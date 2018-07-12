;;;; This file contains the code for the program interface.

(ql:quickload :cl-dbi)
(ql:quickload :quri)

(defparameter *REMOTE-DB-PATH* "/data/data/com.google.android.apps.fireball/databases/fireball.db")
(defparameter *TEMP* (uiop:default-temporary-directory))

(defparameter *output-dir* (merge-pathnames (uiop:parse-unix-namestring "../acs_report/") (uiop:getcwd)))
(defparameter *db* nil)

;; This only works on rooted devices. To change that I would need to use `adb backup` then untar that archive.
(defun adb-pull-database ()
  "Pulls the message database from the phone by calling `adb pull`"
  (uiop:run-program (list "adb" "pull" *REMOTE-DB-PATH* (namestring *TEMP*)) :output t))
(defun list-conversations ()
  "Queries the database and returns tuples of the conversation ID's and their members"
  (let ((convos (fetch-sql "SELECT _id,default_name FROM conversations")))
    (mapcar (lambda (row) (cons (lookup :|_id| row) (lookup :|default_name| row))) convos)))

(defun list-messages (convo-id)
  "Curates all of the messages that belong to a particular conversation"
  (let ((msgs (fetch-sql (concatenate 'string "SELECT * FROM messages WHERE conversation_id=" (princ-to-string convo-id)))))
    (mapcar #'process-message msgs)))

(defun fetch-sql (sql)
  "Takes a SQL query and returns the result in alist form"
  (unless *db* (setf *db* (dbi:connect :sqlite3 :database-name (merge-pathnames *TEMP* "fireball.db"))))
  (mapcar #'kvlist-to-alist (dbi:fetch-all (dbi:execute (dbi:prepare *db* sql)))))

(defun kvlist-to-alist (lst)
  "Converts a list in (:key val :key2 val2) form to an alist"
  (labels ((convert (alist kvlist)
	     (if kvlist
		 (convert (cons (cons (car kvlist) (cadr kvlist)) alist) (nthcdr 2 kvlist))
		 alist)))
    (convert () lst)))

(defun lookup (key alist)
  "Returns only the resultant value of an assoc call"
  (cdr (assoc key alist)))

(defun process-message (msg)
  "Takes a message and checks its content type before processing it appropriately"
  (cond ((equal (msg-type msg) "text/plain") (message-builder :text msg :|text|))
	((equal (msg-type msg) "application/sticker") (message-builder :sticker msg :|uri|))
	((member (msg-type msg) '("image/jpeg" "image/png") :test #'equal) (message-builder :image msg :|uri|))
	(t (message-builder :unsupported msg :|content_type|))))
 
(defun resolve-sender-id (sender-id)
  "Takes a sender ID, references the database, and returns the sender's name"
  (let ((sender (fetch-sql (concatenate 'string "SELECT contact_display_name FROM fireball_users WHERE _id=" (princ-to-string sender-id)))))
    (lookup :|contact_display_name| (car sender))))

(defun adb-fetch-media (msg)
  "This will (eventually) take uri's in messages and fetch the relevant media"
  (when (member (car msg) '(:image :sticker))
    (let ((media-dir (merge-pathnames "media/" *output-dir*))
	  (uri (format-uri (car (last msg))))) 
      (ensure-directories-exist media-dir)
      (cond ((eq (car msg) :image)
	     (unless (probe-file (merge-pathnames media-dir (file-namestring uri))) (uiop:run-program (list "adb" "pull" uri (namestring media-dir)) :output t)))
	    ((eq (car msg) :sticker)
	     (let ((temp-file (namestring (merge-pathnames *TEMP* (file-namestring uri))))
		   (conv-file (namestring (merge-pathnames media-dir (concatenate 'string (pathname-name uri) ".png"))))
		   (copy-file (namestring (merge-pathnames media-dir (concatenate 'string (pathname-name uri) ".webp")))))
	       (unless (or (probe-file copy-file) (probe-file conv-file))
		 (uiop:run-program (list "adb" "pull" uri (namestring *TEMP*)) :output t)
		 (handler-case (uiop:run-program (list "convert" temp-file conv-file))
		   (error () (uiop:run-program (list "cp" temp-file copy-file)))))))))))

(defun unix-to-date (unix-milli)
  (let* ((unix (round (/ unix-milli 1000)))
	 (universal (+ unix (encode-universal-time 0 0 0 1 1 1970 0))))
    (multiple-value-bind (second minute hour date month year) (decode-universal-time universal)
      (format nil "~D.~D.~D (~2,'0D:~2,'0D:~2,'0D)" year month date hour minute second))))

(defun message-builder (type msg &rest fields)
  (let ((header (list type (resolve-sender-id (lookup :|sender_id| msg)) (unix-to-date (lookup :|received_timestamp| msg)))))
    (append header (mapcar (lambda (sym) (lookup sym msg)) fields))))

(defun msg-type (msg)
  (lookup :|content_type| msg))

(defun format-uri (uri-str)
  (let ((sdcard "/storage/emulated/0/")
	(uri (quri:url-decode (quri:uri-path (quri:uri uri-str)))))
    (if (search sdcard uri)
	(concatenate 'string "/sdcard/" (subseq uri (length sdcard)))
	uri)))
