;;;; This file contains the code for talking with the database and converting data to a more usable representation

(defparameter *db* nil)

(defun db-connect ()
  (unless *db* (setf *db* (dbi:connect :sqlite3 :database-name (merge-pathnames *TEMP* "fireball.db")))))

(defun db-free ()
  (when *db* (dbi:disconnect *db*) (setf *db* nil)))

(defun list-conversations ()
  "Queries the database and returns tuples of the conversation ID's and their members"
  (let ((convos (fetch-sql "SELECT _id,default_name FROM conversations")))
    (mapcar (lambda (row) (cons (lookup :|_id| row) (lookup :|default_name| row))) convos)))

(defun list-messages (convo-id &optional start end)
  "Curates all of the messages that belong to a particular conversation (optionally in a time range)"
  (let* ((range-sql (cond ((and start end) (concatenate 'string " AND received_timestamp BETWEEN " (write-to-string (date-to-unix start))
						        	" AND " (write-to-string (date-to-unix end))))
			  (start (concatenate 'string " AND received_timestamp>" (write-to-string (date-to-unix start))))
			  (t "")))
	 (msgs (fetch-sql (concatenate 'string "SELECT * FROM messages WHERE conversation_id=" (write-to-string convo-id) range-sql))))
    (remove-if (lambda (msg) (member (car msg) '(:empty :unsupported)))
	       (mapcar #'process-message msgs))))

(defun fetch-sql (sql)
  "Takes a SQL query and returns the result in alist form"
  (mapcar #'kvlist-to-alist (dbi:fetch-all (dbi:execute (dbi:prepare *db* sql)))))

(defun process-message (msg)
  "Takes a message and checks its content type before processing it appropriately"
  (cond ((equal (msg-type msg) "text/plain") (message-builder :text msg :|text_size| :|text|))
	((equal (msg-type msg) "application/gbot") (message-builder :text msg :|text_size| :|fallback_text|))
	((equal (msg-type msg) "application/sticker") (message-builder :sticker msg :|uri|))
	((search "image" (msg-type msg)) (message-builder :image msg :|uri|))
	((search "video" (msg-type msg)) (message-builder :video msg :|uri|))
	(t (message-builder :unsupported msg :|content_type|))))
 
(defun resolve-sender-id (sender-id)
  "Takes a sender ID, references the database, and returns the sender's name"
  (let ((sender (fetch-sql (concatenate 'string "SELECT * FROM fireball_users WHERE _id=" (write-to-string sender-id)))))
    (or (ensure-string-content (lookup :|contact_display_name| (car sender)))
	(ensure-string-content (lookup :|profile_display_name| (car sender))))))

;; Extend this so that if one of the fields is a symbol, look it up; otherwise just use the field value.
(defun message-builder (type msg &rest fields)
  "Takes a type, raw message, and list of relevent fields. The then returns a list of the type and then the values of the selected fields (plus a timestamp and sender id)"
  (let ((header (list type (resolve-sender-id (lookup :|sender_id| msg)) (unix-to-date (lookup :|received_timestamp| msg))))
	(body (mapcar (lambda (sym) (lookup sym msg)) fields)))
    (if (every (lambda (data) (or (not (typep data 'string)) (ensure-string-content data))) body)
	(append header body)
	(list :empty))))
