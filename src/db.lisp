;;;; This file contains the code for talking with the database and converting data to a more usable representation

(defparameter *db* nil)

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

(defun message-builder (type msg &rest fields)
  "Takes a type, raw message, and list of relevent fields. The then returns a list of the type and then the values of the selected fields (plus a timestamp and sender id)"
  (let ((header (list type (resolve-sender-id (lookup :|sender_id| msg)) (unix-to-date (lookup :|received_timestamp| msg)))))
    (append header (mapcar (lambda (sym) (lookup sym msg)) fields))))
