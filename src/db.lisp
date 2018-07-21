;;;; This file contains the code for talking with the database and converting data to a more usable representation

(defparameter *db* nil "Stores the reference for the database connection.")

(defun db-connect ()
  "Connects to the Allo database and populates `*db*`."
  (unless *db* (setf *db* (dbi:connect :sqlite3 :database-name (merge-pathnames *TEMP* "fireball.db")))))

(defun db-free ()
  "Disconnects from the database and clears `*db*`."
  (when *db* (dbi:disconnect *db*) (setf *db* nil)))

(defun list-conversations ()
  "Queries the database and returns tuples of the conversation ID's and their members."
  (let ((convos (fetch-sql "SELECT _id,default_name FROM conversations")))
    (mapcar (lambda (row) (cons (lookup :|_id| row) (lookup :|default_name| row))) convos)))

(defun list-messages (convo-id &optional start end)
  "Curates all of the messages that belong to a particular conversation (optionally in a time range)."
  (let* ((range-sql (cond ((and start end) (concatenate 'string " AND server_timestamp BETWEEN " (write-to-string (date-to-unix start)) " AND " (write-to-string (date-to-unix end))))
			  (start (concatenate 'string " AND server_timestamp>" (write-to-string (date-to-unix start))))
			  (t "")))
	 (msgs (fetch-sql (concatenate 'string "SELECT * FROM messages WHERE conversation_id=" (write-to-string convo-id) range-sql))))
    (remove-if-not #'msg-valid (mapcar #'process-message msgs))))

(defun fetch-sql (sql)
  "Takes a SQL query and returns the result in alist form."
  (mapcar #'kvlist-to-alist (dbi:fetch-all (dbi:execute (dbi:prepare *db* sql)))))

(defun process-message (msg)
  "Takes a message and checks its content type before processing it appropriately."
  (cond ((equal (msg-type msg) "text/plain") (message-builder :text msg :|text_size| #'bot-tagged-text))
	((equal (msg-type msg) "application/pdf") (message-builder :document msg :|file_name| :|uri|))
	((equal (msg-type msg) "application/gbot") (message-builder :text msg 16 :|fallback_text|))
	((equal (msg-type msg) "application/sticker") (message-builder :sticker msg :|uri|))
	((equal (msg-type msg) "application/latlong") (message-builder :location msg :|location_lat| :|location_lng| :|location_address|))
	((search "image" (msg-type msg)) (message-builder :image msg :|uri|))
	((search "video" (msg-type msg)) (message-builder :video msg :|uri|))
	((search "audio" (msg-type msg)) (message-builder :audio msg :|uri|))
	(t (message-builder :unsupported msg :|content_type|))))

(defun bot-tagged-text (msg)
  "Takes a message and checks if it was directed at a bot by a human. If so, it tags the message with `@botname`."
  (let ((text (lookup :|text| msg))
	(bot_recv (ensure-string-content (lookup :|bot_destination_id| msg)))
	(bot_send (lookup :|bot_origin_id| msg)))
    (if (and bot_recv (not (equal bot_recv bot_send)))
	(format nil "@~A ~A" bot_recv text)
	text)))

(defun resolve-sender-id (sender-id)
  "Takes a sender ID, references the database, and returns the sender's name."
  (let ((sender (fetch-sql (concatenate 'string "SELECT * FROM fireball_users WHERE _id=" (write-to-string sender-id)))))
    (or (ensure-string-content (lookup :|contact_display_name| (car sender)))
	(ensure-string-content (lookup :|profile_display_name| (car sender))))))

(defun message-builder (type msg &rest fields)
  "Takes a type, raw message, and list of relevent fields. The then returns a list of the type and then the values of the selected fields (plus a timestamp and sender id)."
  (let ((header (list type (resolve-sender-id (lookup :|sender_id| msg)) (unix-to-date (lookup :|server_timestamp| msg)))))
    (flet ((resolve-field (field)
	     (cond ((symbolp field) (lookup field msg))
		   ((functionp field) (funcall field msg))
		   (t field))))
      (append header (mapcar #'resolve-field fields)))))
