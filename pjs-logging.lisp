(in-package #:pjs-logging)

(defclass logging-context ()
  ((path :initarg :path
	 :accessor logging-context-path)
   (mailbox :initarg :mailbox
	    :accessor logging-context-mailbox)))

(defclass message ()
  ((text :initarg :text
	 :accessor message-text)
   (kind :initarg :kind
	 :accessor message-kind)
   (thread :initarg :thread
	   :accessor message-thread)))

(defun make-logging-context (path)
  (let ((mailbox (sb-concurrency:make-mailbox)))
    (push (lambda ()
	    (sb-concurrency:send-message mailbox
					 :quit))
	  sb-ext:*exit-hooks*)
    (sb-thread:make-thread (lambda ()
			     (let* ((pathname (sb-ext:parse-native-namestring path))
				    (exists (cl-fad:file-exists-p pathname))
				    (needs-init (not exists)))
			       (sqlite:with-open-database (db path)
				 (when needs-init
				   (sqlite:execute-non-query db 
							     (sconc "CREATE TABLE messages ("
								    "id INTEGER PRIMARY KEY NOT NULL,"
								    "kind TEXT NOT NULL,"
								    "at TEXT DEFAULT CURRENT_TIMESTAMP NOT NULL,"
								    "thread TEXT NOT NULL,"
								    "message TEXT NOT NULL"
								    ")")))
				 (while t
				   (let ((message (sb-concurrency:receive-message mailbox)))
				     (if (eq message :quit)
					 (return)
					 ;; else
					 (sqlite:execute-non-query db 
								   "INSERT INTO messages (message, thread, kind) VALUES (?,?,?)"
								   (message-text message)
								   (message-thread message)
								   (message-kind message)))))))))
    (make-instance 'logging-context
		   :mailbox mailbox
		   :path path)))

(defun log-message (context kind message args)
  (let ((message (make-instance 'message
				:thread (sb-thread:thread-name sb-thread:*current-thread*)
				:kind (symbol-name kind)
				:text (apply #'format nil message args))))
    (sb-concurrency:send-message (logging-context-mailbox context)
				 message)))

(defmacro def-logger (name context-var)
  `(progn
     (defvar ,context-var nil)
     (defun ,name (kind message &rest args)
       (log-message ,context-var kind message args))))

(defmacro with-logged-backtraces ((logger) &body body)
  `(handler-bind ((error 
		    (lambda (c)
		      (,logger 'error
			       "Received error: ~a~%~w"
			       c
			       (trivial-backtrace:backtrace-string))))
		  (condition
		    (lambda (c)
		      (,logger 'message
			       "Received condition: ~a~%~w"
			       c
			       (trivial-backtrace:backtrace-string)))))
     ,@body))