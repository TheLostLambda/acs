;;;; This file contains the code for the program interface.

(ql:quickload "cl-dbi")

(defconstant REMOTE-DB-PATH "/data/data/com.google.android.apps.fireball/databases/fireball.db")
(defconstant TEMP (uiop:default-temporary-directory))

;; This only works on rooted devices. To change that I would need to use `adb backup` then untar that archive.
(defun adb-pull-database ()
  "Pulls the message database from the phone by calling `adb pull`"
  (uiop:run-program (list "adb" "pull" REMOTE-DB-PATH (uiop:native-namestring TEMP)) :output t))

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
  (dbi:with-connection (conn :sqlite3 :database-name (merge-pathnames TEMP #p"fireball.db"))
    (mapcar #'kvlist-to-alist (dbi:fetch-all (dbi:execute (dbi:prepare conn sql))))))

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
  (cond ((equal (lookup :|content_type| msg) "text/plain")
	 (list :text (lookup :|sender_id| msg) (lookup :|server_timestamp| msg) (lookup :|text| msg)))
	(t "UNSUPPORTED CONTENT TYPE")))

(defun resolve-sender-id (id)
  "Takes a sender ID, references the database, and returns the sender's name"
  )

(defun adb-pull-media (uri)
  "This will (eventually) take uri's in messages and fetch the relevant media"
  ;(uiop:run-program (list "adb" "pull" REMOTE-MEDIA-PATH (uiop:native-namestring TEMP)) :output t))
  )
