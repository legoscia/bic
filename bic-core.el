;;; bic-core.el --- core of the Best IMAP Client     -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Magnus Henoch

;; Author: Magnus Henoch <magnus.henoch@gmail.com>
;; Keywords: mail

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'cl-lib)
(require 'fsm)
(require 'sasl)
(require 'gnutls)
(require 'auth-source)
(require 'password-cache)

(defvar bic-transcript-buffer "*bic-transcript-%s*")

(defvar bic-ignore-tls-errors nil
  "If non-nil, ignore certificate verification errors.")

(defvar bic-send-cleartext-password nil
  "If non-nil, allow sending passwords on unencrypted connections.")

(defvar bic-ignored-capabilities nil
  "Pretend that the server doesn't advertise these capabilities.
This should be a list of strings.  For debugging only.")

(defvar-local bic--issued-markers nil
  "Hash table of markers issued from current buffer.
Ideally, this should be a weak ordered list, but since Emacs Lisp
doesn't have that, we use a weak hash table instead.")

(defvar-local bic--next-issued-marker 0
  "The key for the next marker to be issued.")

(defvar-local bic--next-collected-marker 0
  "Start looking at this key for markers to collect.")

(define-state-machine bic-connection
  :start ((username server connection-type
		    &optional port callback auth-wait)
	  "Start an IMAP connection.
USERNAME is the username to authenticate as.
SERVER is the server to connect to.
CONNECTION-TYPE is one of the following:
- :starttls, connect to port 143 and request encryption
- :plaintls, connect to port 993 and encrypt from the start
- :unencrypted, connect to port 143 without encrypting
PORT, if given, overrides the port derived from CONNECTION-TYPE.

CALLBACK, if given, should be a function taking two arguments.
The first argument is always the FSM (can be compared with `eq').
It will be called with :authenticated as the second argument once
authentication has completed successfully.  It will also be
called with \(:disconnected KEYWORD REASON-STRING\) when the
connection is closed.

If AUTH-WAIT is provided and non-nil, we establish an encrypted
connection, but don't proceed with authentication immediately.
The CALLBACK will be called with a second argument of :auth-wait,
and the caller needs to send :proceed using `fsm-send' to
proceed."
	  (list :connecting
		(list :name (concat username "@" server)
		      :username username
		      :server server
		      :port port
		      :connection-type connection-type
		      :callback (or callback #'ignore)
		      :auth-wait auth-wait))))

(define-enter-state bic-connection :connecting
  (fsm state-data)
  (let* ((server (plist-get state-data :server))
	 (connection-type (plist-get state-data :connection-type))
	 (service (or (plist-get state-data :port)
		      (cl-ecase connection-type
			((:starttls :unencrypted) 143)
			(:plaintls 993))))
	 (buffer (generate-new-buffer (concat "bic-" server))))
    (condition-case e
	(let ((proc (make-network-process
		     :name (concat "bic-" server)
		     :buffer buffer
		     :host server
		     :service service
		     :coding 'binary
		     :nowait t
		     :keepalive t
		     :filter (fsm-make-filter fsm)
		     :sentinel (fsm-make-sentinel fsm))))
	  (buffer-disable-undo buffer)
	  (list (plist-put state-data :proc proc) nil))
      (error
       ;; We can't move directly to a different state in the enter state
       ;; function...
       (kill-buffer buffer)
       (fsm-send fsm (list :connection-failed e server service))
       (list state-data nil)))))

(define-state bic-connection :connecting
  (fsm state-data event _callback)
  (pcase event
    (`(:connection-failed ,e ,server ,service)
     ;; from enter-state-function
     (bic--fail state-data
		:connection-failed
		(format "connection to %s:%s failed: %s"
			server service (error-message-string e))))
    (`(:sentinel ,proc ,string)
     (cond
      ((string-prefix-p "open" string)
       (bic--transcript fsm (format "*** %s Connected to %s\n"
				    (format-time-string "%F %T")
				    (plist-get state-data :server)))
       (cl-ecase (plist-get state-data :connection-type)
	 ((:starttls :unencrypted)
	  ;; Wait for STARTTLS capability etc
	  (list :wait-for-greeting state-data nil))
	 (:plaintls
	  ;; Negotiate TLS immediately
	  (condition-case e
	      (progn
		(bic--negotiate-tls state-data)
		;; No error?  Connection encrypted!
		(list :wait-for-greeting
		      (plist-put
		       (plist-put state-data :encrypted t)
		       :capabilities nil)))
	    (error
	     (bic--fail state-data
			:tls-failure
			(format "Cannot negotiate TLS for %s: %s"
				(plist-get state-data :server)
				(error-message-string e))))))))
      ((or (string-prefix-p "failed" string)
	   (string-prefix-p "deleted" string))
       ;; strip trailing newline
       (when (eq ?\n (aref string (1- (length string))))
	 (setq string (substring string 0 -1)))
       (let* ((contact (process-contact proc))
	      (server (car contact))
	      (service (cadr contact)))
	 (bic--fail state-data
		    :connection-failed
		    (format "connection to %s:%s %s"
			    server service string))))
      (t
       (message "Unknown sentinel event %S" string)
       (list :connecting state-data nil))))
    (:stop
     (bic--fail state-data :stopped "Stopped"))
    (unexpected
     (message "Unexpected event %S" unexpected)
     (list :connecting state-data nil))))

(define-state bic-connection :wait-for-greeting
  (fsm state-data event _callback)
  (pcase event
    (`(:filter ,process ,data)
     (bic--filter process data fsm)
     (list :wait-for-greeting state-data))
    (`(:sentinel ,_process ,reason)
     ;; strip trailing newline
     (when (eq ?\n (aref reason (1- (length reason))))
       (setq reason (substring reason 0 -1)))
     (bic--fail state-data :connection-closed reason))
    (`(:line ,line)
     (pcase (bic--parse-greeting line)
       (`(:ok ,capabilities ,_greeting-text)
	(if (null capabilities)
	    (list :wait-for-capabilities state-data)
	  (plist-put state-data :capabilities capabilities)
	  (bic--advance-connection-state fsm state-data capabilities)))
       (`(:bye ,text)
	(bic--fail state-data
		   :server-disconnect
		   (format "Server wants to disconnect: %s" text)))))
    (:stop
     (bic--fail state-data :stopped "Stopped"))
    (event
     (message "Got event %S" event)
     (list :wait-for-greeting state-data))))

(define-enter-state bic-connection :wait-for-capabilities
  (fsm state-data)
  (bic--send fsm "caps CAPABILITY\r\n")
  (list state-data nil))

(define-state bic-connection :wait-for-capabilities
  (fsm state-data event _callback)
  (pcase event
    (`(:filter ,process ,data)
     (bic--filter process data fsm)
     (list :wait-for-capabilities state-data))
    (`(:sentinel ,_process ,reason)
     ;; strip trailing newline
     (when (eq ?\n (aref reason (1- (length reason))))
       (setq reason (substring reason 0 -1)))
     (bic--fail state-data :connection-closed reason))
    (`(:line ,line)
     (pcase (bic--parse-line line)
       (`("*" "CAPABILITY" . ,capability-strings)
	(let ((capabilities (bic--parse-capabilities capability-strings)))
	  (list :wait-for-capabilities
		(plist-put state-data :capabilities capabilities))))
       (`("caps" :ok . ,_)
	(bic--advance-connection-state
	 fsm state-data
	 (plist-get state-data :capabilities)))
       (_
	(bic--fail state-data
		   :unexpected-input
		   (format "Unexpected: %s" line)))))
    (:stop
     (bic--fail state-data :stopped "Stopped"))
    (event
     (message "Got event %S" event)
     (list :wait-for-capabilities state-data))))

(define-state bic-connection :wait-for-auth-proceed
  (fsm state-data event _callback)
  (pcase event
    (:proceed
     (plist-put state-data :auth-wait nil)
     (bic--advance-connection-state
      fsm state-data
      (plist-get state-data :capabilities)))
    (`(:sentinel ,_process ,reason)
     ;; strip trailing newline
     (when (eq ?\n (aref reason (1- (length reason))))
       (setq reason (substring reason 0 -1)))
     (bic--fail state-data :connection-closed reason))
    (:stop
     (bic--fail state-data :stopped "Stopped"))))

(defun bic--advance-connection-state (fsm state-data capabilities)
  (cond
   ((and (not (plist-get state-data :encrypted))
	 (not (eq (plist-get state-data :connection-type) :unencrypted)))
    (if (member "STARTTLS" capabilities)
	(progn
	  (bic--send fsm "starttls STARTTLS\r\n")
	  (list :wait-for-starttls-response state-data))
      (bic--fail state-data
		 :starttls-not-available
		 "STARTTLS not available!")))
   ((and (not (plist-get state-data :authenticated))
	 (plist-get state-data :auth-wait))
    ;; Our caller wants us to wait before proceeding with
    ;; authentication.
    (funcall (plist-get state-data :callback) fsm :auth-wait)
    (list :wait-for-auth-proceed state-data))
   ((not (plist-get state-data :authenticated))
    (let* ((server-mechanisms (cdr (assq :auth capabilities)))
	   (mechanism (sasl-find-mechanism server-mechanisms)))
      (cond
       ((null mechanism)
	(bic--fail state-data
		   :sasl-mechanism-not-found
		   (format "No suitable mechanism found!  We support %s, server supports %s"
			   sasl-mechanisms server-mechanisms)))
       ((and (not bic-send-cleartext-password)
	     (not (plist-get state-data :encrypted))
	     (member (sasl-mechanism-name mechanism) '("PLAIN" "LOGIN")))
	(bic--fail state-data
		   :sasl-cleartext-not-allowed
		   (format "Cleartext authentication not allowed!  Server offers %s"
			   server-mechanisms)))
       (t
	(let* ((client
		(sasl-make-client
		 mechanism
		 (plist-get state-data :username)
		 "imap"
		 (plist-get state-data :server)))
	       (sasl-read-passphrase (bic--read-passphrase-function state-data))
	       (step (catch :bic-sasl-abort (sasl-next-step client nil))))
	  (pcase step
	    (:quit
	     (bic--fail state-data
			:authentication-abort
			"User quit during IMAP authentication"))
	    (:timeout
	     (bic--fail state-data
			:authentication-abort
			"Timeout waiting for password during IMAP authentication"))
	    (_
	     ;; XXX: we can't send the AUTHENTICATE command here, because
	     ;; sending data over a network connection means that we can
	     ;; receive data as well, which causes a race condition
	     ;; whereby the filter function being called with the server
	     ;; response before we've moved on to the :sasl-auth state.
	     ;; Thus we send the AUTHENTICATE command in the enter
	     ;; function instead.
	     (list :sasl-auth (plist-put
			       (plist-put state-data :sasl-client client)
			       :sasl-step step)))))))))
   (t
    (list :authenticated state-data))))

(define-state bic-connection :wait-for-starttls-response
  (fsm state-data event _callback)
  (pcase event
    (`(:filter ,process ,data)
     (bic--filter process data fsm)
     (list :wait-for-starttls-response state-data))
    (`(:sentinel ,_process ,reason)
     ;; strip trailing newline
     (when (eq ?\n (aref reason (1- (length reason))))
       (setq reason (substring reason 0 -1)))
     (bic--fail state-data :connection-closed reason))
    (`(:line ,line)
     (pcase (bic--parse-line line)
       (`("starttls" :ok . ,_)
	(condition-case e
	    (progn
	      (bic--negotiate-tls state-data)
	      ;; No error?  Connection encrypted!
	      ;; Forget capabilities and ask again on encrypted connection.
	      (list :wait-for-capabilities
		    (plist-put
		     (plist-put state-data :encrypted t)
		     :capabilities nil)))
	  (error
	   (bic--fail state-data
		      :tls-failure
		      (format "Cannot negotiate STARTTLS for %s: %s"
			      (plist-get state-data :server)
			      (error-message-string e))))))
       (`("starttls" :bad . ,plist)
	(bic--fail state-data
		   :tls-failure
		   (format "Cannot negotiate STARTTLS: %s"
			   (plist-get plist :text))))
       (_
	(bic--fail state-data
		   :tls-failure
		   (format "Unexpected response to STARTTTLS command: %s" line)))))
    (:stop
     (bic--fail state-data :stopped "Stopped"))
    (event
     (message "Got event %S" event)
     (list :wait-for-starttls-response state-data))))

(define-enter-state bic-connection :sasl-auth
  (fsm state-data)
  (let* ((client (plist-get state-data :sasl-client))
	 (mechanism (sasl-client-mechanism client))
	 (step (plist-get state-data :sasl-step)))
    (bic--send
     fsm
     (concat "auth AUTHENTICATE " (sasl-mechanism-name mechanism)
	     (when (and (member "SASL-IR" (plist-get state-data :capabilities))
			(sasl-step-data step))
	       ;; We can send an "initial response", saving a
	       ;; roundtrip.
	       (concat " "
		       (propertize
			(base64-encode-string (sasl-step-data step) t)
			:sensitive t)))
	     "\r\n")))
  (list state-data nil))

(define-state bic-connection :sasl-auth
  (fsm state-data event callback)
  (pcase event
    (`(:filter ,process ,data)
     (bic--filter process data fsm :sensitive (apply-partially #'string-prefix-p "+ "))
     (list :sasl-auth state-data))
    (`(:sentinel ,_process ,reason)
     ;; strip trailing newline
     (when (eq ?\n (aref reason (1- (length reason))))
       (setq reason (substring reason 0 -1)))
     (bic--fail state-data :connection-closed reason))
    (`(:line ,line)
     (pcase (bic--parse-line line)
       (`("+" ,data)
	(let ((client (plist-get state-data :sasl-client))
	      (step (plist-get state-data :sasl-step))
	      (sasl-read-passphrase (bic--read-passphrase-function state-data)))
	  ;; If this is the first message from the server, and it is
	  ;; empty, and the chosen mechanism requires the client to
	  ;; send data first, then we shouldn't move to the next step
	  ;; here.
	  ;;
	  ;; There is no way to ask the Emacs SASL library about
	  ;; whether the client should send data first, so let's take
	  ;; an empty message from the server as our cue.
	  (pcase
	      (catch :bic-sasl-abort
		(unless (and (zerop (length data))
			     (null (plist-get state-data :sasl-sent-message)))
		  (sasl-step-set-data step (base64-decode-string data))
		  (setq step (sasl-next-step client step))
		  nil))
	    (`nil
	     ;; Update state-data before sending response, to avoid a race
	     ;; condition.  plist-put only requires reassignment if the
	     ;; list was initially empty, which we by now know is not the
	     ;; case.
	     (plist-put state-data :sasl-step step)
	     (plist-put state-data :sasl-sent-message t)
	     (bic--send fsm (concat (base64-encode-string (or (sasl-step-data step) "") t) "\r\n")
			:sensitive t)
	     ;; XXX: check local success/failure, for mechanisms that
	     ;; simultaneously authenticate the server
	     (list :sasl-auth state-data))
	    (:quit
	     (bic--fail state-data
			:authentication-abort
			"User quit during IMAP authentication"))
	    (:timeout
	     (bic--fail state-data
			:authentication-abort
			"Timeout waiting for password during IMAP authentication"))
	    (other
	     (bic--fail state-data
			:unexpected-error
			(format "Unexpected result of SASL step: %S" other))))))
       (`("auth" :ok . ,plist)
	;; XXX: check local success/failure here too
	(let ((new-capabilities
	       (when (string= (plist-get plist :code) "CAPABILITY")
		 (bic--parse-capabilities (split-string (plist-get plist :data)))))
	      (text (plist-get plist :text)))
	  (plist-put state-data :authenticated t)
	  (plist-put state-data :capabilities new-capabilities)
	  (plist-put state-data :sasl-client nil)
	  (plist-put state-data :sasl-step nil)
	  (when (plist-get state-data :password-save-function)
	    ;; Ask the user about saving the password.
	    (with-local-quit
	      (funcall (plist-get state-data :password-save-function)))
	    (plist-put state-data :password-save-function nil))
	  (if new-capabilities
	      ;; The server saved us a roundtrip and sent
	      ;; capabilities in the OK message.
	      (list :authenticated state-data)
	    ;; Need to ask the server for capabilities.
	    (list :wait-for-capabilities state-data))))
       (`("auth" :no . ,plist)
	;; TODO: ask for better password?
	(bic--fail state-data
		   :authentication-failed
		   (format "IMAP authentication failed: %s"
			   (plist-get plist :text))))
       (`("auth" :bad . ,plist)
	;; This shouldn't happen
	(bic--fail state-data
		   :unexpected-error
		   (format "Unexpected IMAP authentication error: %s"
			   (plist-get plist :text))))
       (`("*" ,_ . ,_)
	;; Untagged responses can arrive at any time (2.2.2, RFC
	;; 3501).  Let's ignore it and hope it wasn't important.
	(list :sasl-auth state-data))
       (_
	(bic--fail state-data
		   :unexpected-input
		   (format "Unexpected input: %s" line)))))
    (:stop
     (bic--fail state-data :stopped "Stopped"))
    (event
     (message "Got event %S" event)
     (list :sasl-auth state-data))))

(defun bic--read-passphrase-function (state-data)
  (lambda (prompt)
    (let ((auth-source-result
	   (with-timeout (60 :timeout)
	     (or
	      (with-local-quit
		(auth-source-search
		 :user (plist-get state-data :username)
		 :host (plist-get state-data :server)
		 :port
		 (let ((symbolic (cl-ecase (plist-get state-data :connection-type)
				   ((:starttls :unencrypted)
				    "imap")
				   (:plaintls
				    "imaps")))
		       (numeric (plist-get state-data :port)))
		   (if numeric
		       (list symbolic numeric)
		     symbolic))
		 :max 1
		 :require '(:secret)
		 :create t))
	      (and quit-flag :quit)))))
      (pcase auth-source-result
	(:timeout
	 (throw :bic-sasl-abort :timeout))
	(:quit
	 (throw :bic-sasl-abort :quit))
	(`(,found . ,_)
	 (let ((secret (plist-get found :secret))
	       (save-function (plist-get found :save-function)))
	   (plist-put state-data :password-save-function save-function)
	   ;; Copy the password, as sasl.el wants to erase it.
	   (copy-sequence (if (functionp secret) (funcall secret) secret))))
	(other
	 (throw :bic-sasl-abort (cons :unexpected other)))))))

(define-enter-state bic-connection :authenticated
  (fsm state-data)
  (funcall (plist-get state-data :callback) fsm :authenticated)
  (list state-data nil))

(define-state bic-connection :authenticated
  (fsm state-data event callback)
  (pcase event
    (`(:cmd ,cmd ,early-callbacks)
     (let* ((tag-number (or (plist-get state-data :next-tag) 0))
	    (next-tag (1+ tag-number))
	    (tag-string (number-to-string tag-number))
	    (pending-commands
	     ;; Need to keep commands in the correct order.  We
	     ;; shouldn't have that many pending commands, so
	     ;; appending all the time should be fine.
	     (append (plist-get state-data :pending-commands)
		     (list (list tag-string early-callbacks callback)))))
       (plist-put state-data :pending-commands pending-commands)
       (plist-put state-data :next-tag next-tag)
       (bic--send fsm (concat tag-string " " cmd "\r\n"))
       (list :authenticated state-data)))

    (`(:filter ,process ,data)
     (bic--filter process data fsm)
     (list :authenticated state-data))

    (`(:line ,line)
     (bic--handle-line state-data line)
     (list :authenticated state-data))

    (`(:sentinel ,_process ,reason)
     ;; strip trailing newline
     (when (eq ?\n (aref reason (1- (length reason))))
       (setq reason (substring reason 0 -1)))
     (bic--fail state-data :connection-closed reason))

    (:stop
     (bic--fail state-data :stopped "Stopped"))))

(defun bic--handle-line (state-data line)
  (pcase (bic--parse-line line)
    (`("*" . ,rest)
     (unless
	 ;; maybe send results early
	 (catch 'handled
	   (dolist (early-callback
		    (cl-second (car (plist-get state-data :pending-commands))))
	     (when (equal (nth (cl-first early-callback) rest)
			  (cl-second early-callback))
	       (funcall (cl-third early-callback) rest)
	       (throw 'handled t))))
       ;; ...otherwise just store the line in the state.
       (plist-put state-data :response-acc
		  (cons rest (plist-get state-data :response-acc)))))
    (`("+" . ,_rest)
     ;; Continuation response.  XXX: do something sensible
     )
    (`(,tag ,type . ,rest)
     (let* ((pending-commands (plist-get state-data :pending-commands))
	    (entry (assoc tag pending-commands))
	    (command-callback (cl-third entry))
	    (new-pending-commands (delq entry pending-commands))
	    (response-acc (plist-get state-data :response-acc)))
       (plist-put state-data :response-acc nil)
       (plist-put state-data :pending-commands new-pending-commands)
       (funcall command-callback (list type rest response-acc))))
    (_
     (fsm-debug-output "Unexpected line: '%s'" line))))

(defun bic-command (fsm command callback &optional early-callbacks)
  "Send an IMAP command.
COMMAND is a string containing an IMAP command minus the tag.

CALLBACK is a function that takes one argument of the form
\(RESPONSE TEXT RESPONSE-LINES), where RESPONSE is a string
containing the response type, typically \"OK\", \"NO\" or
\"BAD\", TEXT is the rest of the tagged response line, and
RESPONSE-LINES is a list of (TYPE TEXT) entries, one for
each untagged response line.

The callback function will be called when the command has
finished.  There is no immediate response.

EARLY-CALLBACKS is a list with elements of the form:

  (N RESPONSE-NAME FUNCTION)

where N is an integer and RESPONSE-NAME is a string.
If the Nth word of a response line for this command is `equal'
to RESPONSE-NAME, then FUNCTION is called with the response
line as the only argument, and the response line in question is
not included in the final response.  N starts at 0, and does not
include the leading \"*\" tag."
  (fsm-send fsm `(:cmd ,command ,early-callbacks) callback))

(defun bic--fail (state-data keyword reason)
  (plist-put state-data :fail-keyword keyword)
  (plist-put state-data :fail-reason reason)
  (list nil state-data))

(define-enter-state bic-connection nil
  (fsm state-data)
  ;; Delete the connection just to be sure it's gone.
  (let ((proc (plist-get state-data :proc)))
    (when (processp proc)
      (let ((buffer (process-buffer proc)))
	(delete-process proc)
	(when (buffer-live-p buffer)
	  (kill-buffer buffer)))))
  (let ((callback (plist-get state-data :callback))
	(fail-keyword (or (plist-get state-data :fail-keyword)
			  :unknown-reason))
	(fail-reason (or (plist-get state-data :fail-reason)
			 "Unexpected error")))
    (bic--transcript fsm (format "*** %s Connection closed: %s\n"
				 (format-time-string "%F %T")
				 fail-reason))
    (funcall callback fsm (list :disconnected fail-keyword fail-reason)))
  (list nil nil))

(define-state bic-connection nil
  (_fsm state-data _event _callback)
  ;; Ignore all events
  (list nil state-data))

(defun bic--negotiate-tls (state-data)
  (gnutls-negotiate :process (plist-get state-data :proc)
		    :hostname (plist-get state-data :server)
		    :verify-hostname-error (not bic-ignore-tls-errors)
		    :verify-error (not bic-ignore-tls-errors)))

(defvar-local bic--unread-start-marker nil)

(defvar-local bic--literal-start-marker nil)

(defvar-local bic--literal-expected-length nil)

(defvar-local bic--line-acc nil)

(cl-defun bic--filter (process data fsm &key sensitive)
  (with-current-buffer (process-buffer process)
    (unless bic--unread-start-marker
      (setq bic--unread-start-marker (point-min-marker)))

    (goto-char (point-max))
    (insert data)

    (stop-process process)
    ;; Handle input as long as there is something left to handle.
    ;; bic--read-input should return t in this case, and move either
    ;; bic--unread-start-marker or bic--literal-start-marker.
    ;; Double-check that we don't get stuck in an infinite loop.
    (unwind-protect
	(cl-flet
	    ((current-progress
	      ()
	      (list (and bic--unread-start-marker
			 (marker-position bic--unread-start-marker))
		    (and bic--literal-start-marker
			 (marker-position bic--literal-start-marker)))))
	  (let ((previous (current-progress)))
	    (while (bic--read-input fsm process sensitive)
	      (when (equal previous (current-progress))
		(error "no progress"))
	      (setq previous (current-progress)))))
      (continue-process process))))

(defun bic--read-input (fsm process sensitive)
  "Read what the server sent, and send as :line messages to FSM.
Keep calling this function until it returns nil."
  (cond
   ((null bic--literal-start-marker)
    ;; Find complete lines, terminated by CRLF
    (goto-char bic--unread-start-marker)
    (when (search-forward "\r\n" nil t)
      (let* ((line-end (match-end 0))
	     (received-line (buffer-substring
			     bic--unread-start-marker
			     (match-beginning 0))))
	(bic--transcript
	 fsm
	 (concat "S: "
		 (if (and sensitive (funcall sensitive received-line))
		     "<omitted>"
		   received-line)
		 "\n"))
	;; Does a literal start on this line?
	(if (string-match "{\\([0-9]+\\)}$" received-line)
	    (progn
	      (push (substring received-line 0 (match-beginning 0))
		    bic--line-acc)
	      (setq bic--literal-start-marker (copy-marker line-end))
	      (setq bic--literal-expected-length
		    (string-to-number (match-string 1 received-line)))
	      t)
	  ;; Send the line as an event to the FSM
	  (let ((line (nreverse (cons received-line bic--line-acc))))
	    (setq bic--line-acc nil)
	    (set-marker bic--unread-start-marker line-end)
	    (fsm-send fsm (list :line line))
	    t)))))
   ((and bic--literal-start-marker
	 (>= (- (point-max) bic--literal-start-marker)
	     bic--literal-expected-length))
    ;; The literal is complete.  Save the markers in our list, and
    ;; keep parsing.
    (bic--transcript fsm (format "S: <%d bytes omitted>\n" bic--literal-expected-length))
    (let ((literal-end (+ bic--literal-start-marker bic--literal-expected-length)))
      (push (cons (bic--issue-marker bic--literal-start-marker)
		  (bic--issue-marker literal-end))
	    bic--line-acc)
      (setq bic--literal-start-marker nil)
      (set-marker bic--unread-start-marker literal-end)
      ;; TODO: is this too often?
      (bic--prune-old-literals)
      t))))

(defun bic--issue-marker (value)
  "Create a marker pointing at VALUE, and record it.
VALUE must be greater than any marker previously issued."
  (unless bic--issued-markers
    (setq bic--issued-markers (make-hash-table :weakness 'value)))
  (let ((marker (copy-marker value)))
    (prog1
	marker
      (puthash bic--next-issued-marker marker bic--issued-markers)
      (cl-incf bic--next-issued-marker))))

(defun bic--prune-old-literals ()
  "Remove data before the first marker we know about."
  ;; We send chunks of data to the client in the form of marker pairs,
  ;; but we'd like to know when the client is done with the data, so
  ;; that we can delete the data from the connection buffer and
  ;; prevent it from growing indefinitely.  We accomplish this by
  ;; keeping the markers in a weak hash table, such that the entries
  ;; are removed when the markers are garbage collected.
  ;; Alternatively, the client can explicitly make the markers point
  ;; nowhere, which we explicitly check for.
  ;;
  ;; Since a hash table is not an ordered list, we keep two "indexes":
  ;; one for the next key to use when inserting, and one for the next
  ;; pruning candidate.
  (while (and
	  ;; In principle, we should check for integer
	  ;; overflow/wraparound, but even on a 32-bit Emacs this
	  ;; should let you download 134 million messages on a single
	  ;; connection before you run into trouble...
	  (< bic--next-collected-marker bic--next-issued-marker)
	  (let ((maybe-marker (gethash bic--next-collected-marker bic--issued-markers)))
	    ;; If the marker has been garbage collected, it won't be
	    ;; in our weak hash table anymore:
	    (or (null maybe-marker)
		;; If it has been explicitly cleared, remove it from
		;; the table.
		(when (null (marker-position maybe-marker))
		  (remhash bic--next-collected-marker bic--issued-markers)
		  t))))
    (cl-incf bic--next-collected-marker))
  (let ((delete-until
	 (or (gethash bic--next-collected-marker bic--issued-markers)
	     bic--unread-start-marker)))
    (delete-region (point-min) delete-until)))

(cl-defun bic--send (fsm string &key sensitive)
  (bic--transcript
   fsm
   (concat "C: "
	   (if sensitive
	       "<omitted>"
	     (let* ((trimmed
		     (if (string= (substring string -2) "\r\n")
			 (substring string 0 -2)
		       string))
		    ;; TODO: we assume that the string starts
		    ;; "non-sensitive", and switches to "sensitive"
		    ;; throughout.
		    (sensitive-from (next-single-property-change 0 :sensitive trimmed)))
	       (if sensitive-from
		   (concat (substring trimmed 0 sensitive-from) "<omitted>")
		 trimmed)))
	   "\n"))

  (send-string (plist-get (fsm-get-state-data fsm) :proc) string))

;; Defined in view.el
(defvar view-no-disable-on-exit)

(defun bic--transcript (fsm string)
  (with-current-buffer (get-buffer-create (format bic-transcript-buffer (plist-get (fsm-get-state-data fsm) :name)))
    (unless (derived-mode-p 'view-mode)
      (view-mode)
      (setq-local view-no-disable-on-exit t))
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
	(insert string)))))

(defun bic--parse-greeting (line)
  (pcase (bic--parse-line line)
    (`("*" ,(and (or :ok :bye) type) . ,plist)
     (if (string= "CAPABILITY" (plist-get plist :code))
	 (list type (bic--parse-capabilities
		     (split-string (plist-get plist :data)))
	       (plist-get plist :text))
       (list type nil (plist-get plist :text))))
    ;; TODO: PREAUTH
    (_
     (error "Unexpected greeting: %s" line))))


(defun bic--parse-capabilities (strings)
  (let (capabilities auth)
    (dolist (capability strings)
      (if (string-prefix-p "AUTH=" capability)
	  (push (substring capability 5) auth)
	(push capability capabilities)))
    (cons (cons :auth auth) capabilities)))

(cl-defun bic--parse-line (line
			   &key (line-start t) ((:start-at i) 0) closing-parenthesis
			   &aux tokens)
  (when line-start
    (cond
     ((not (stringp (car line)))
      ;; No empty lines, or lines starting with a literal
      (cl-return-from bic--parse-line :unexpected))
     ((string-prefix-p "+" (car line))
      ;; Continuation line
      (cl-return-from bic--parse-line
	(cl-list* "+"
		  (if (< (length (car line)) 2)
		      ;; In principle, this should always be followed by a space.
		      ;; In practice, Exchange sends just a plus sign on a line.
		      ""
		    (substring (car line) 2))
		  (cdr line))))
     ;; If the line starts with a tag and one of the words OK, NO, BAD,
     ;; BYE or PREAUTH, then the rest of the line should be parsed as
     ;; resp-text.
     ;; XXX: tag is `any ASTRING-CHAR expect "+"'
     ((string-match
       "^\\([^ +]+\\) \\(OK\\|NO\\|BAD\\|BYE\\|PREAUTH\\) \\(.*\\)$"
       (car line))
      (cl-return-from bic--parse-line
	(if (cdr line)
	    ;; Can't have literal on lines with resp-text
	    :unexpected
	  (let ((tag (match-string 1 (car line)))
		(type (match-string 2 (car line)))
		(rest (match-string 3 (car line))))
	    (cons tag (bic--parse-resp-text type rest))))))))

  (while line
    (cond
     ((consp (car line))
      (push (pop line) tokens))
     ((or (null i) (>= i (length (car line))))
      (pop line)
      (setq i 0))
     (t
      (cl-case (aref (car line) i)
	(?\"
	 (cl-destructuring-bind (string new-i)
	     (bic--parse-quoted-string (car line) i)
	   (push string tokens)
	   (setq i new-i)))
	((?\) ?\])
	 (if (not (eq (aref (car line) i) closing-parenthesis))
	     (error "Unexpected closing parenthesis in %S" line)
	   (cl-return-from bic--parse-line (list (nreverse tokens) line (1+ i)))))
	((?\( ?\[)
	 (let ((closing
		(cl-ecase (aref (car line) i)
		  (?\( ?\))
		  (?\[ ?\]))))

	   (cl-destructuring-bind (subtokens new-line new-i)
	       (bic--parse-line
		line :line-start nil :start-at (1+ i) :closing-parenthesis closing)
	     (push subtokens tokens)
	     (setq line new-line
		   i new-i))))
	(?\s
	 ;; We're being liberal here, accepting superfluous spaces.
	 (cl-incf i))
	(t
	 ;; TODO: can we get away with representing anything else as a
	 ;; string?
	 (let* ((new-i (cl-position-if
			;; TODO: what about "("?
			(lambda (c) (memq c '(?\s ?\) ?\[ ?\])))
			(car line) :start i))
		;; new-i may be nil
		(token (substring (car line) i new-i)))
	   (push token tokens)
	   (setq i new-i)))))))

  (if closing-parenthesis
      (error "Expected closing `%c'" closing-parenthesis)
    (nreverse tokens)))

(defun bic--parse-resp-text (type resp-text)
;;; resp-text       = ["[" resp-text-code "]" SP] text
;;;
;;; resp-text-code  = "ALERT" /
;;;                   "BADCHARSET" [SP "(" astring *(SP astring) ")" ] /
;;;                   capability-data / "PARSE" /
;;;                   "PERMANENTFLAGS" SP "("
;;;                   [flag-perm *(SP flag-perm)] ")" /
;;;                   "READ-ONLY" / "READ-WRITE" / "TRYCREATE" /
;;;                   "UIDNEXT" SP nz-number / "UIDVALIDITY" SP nz-number /
;;;                   "UNSEEN" SP nz-number /
;;;                   atom [SP 1*<any TEXT-CHAR except "]">]
  ;; TODO: find right amount of greediness.  Currently, we assume that
  ;; 'text' does not contain a right square bracket.
  (if (string-match "\\[\\([^]\s]+\\)\\(?: \\([^]]*\\)\\)?\\]\\(?: \\(.*\\)\\)?" resp-text)
      (let ((resp-text-code (match-string 1 resp-text))
	    (resp-text-data (match-string 2 resp-text))
	    (text (match-string 3 resp-text)))
	(list (intern (concat ":" (downcase type)))
	      :code resp-text-code
	      :data resp-text-data
	      :text text))
    (list (intern (concat ":" (downcase type))) :text resp-text)))

(defun bic--parse-quoted-string (line start)
  "Parse a quoted string in LINE, starting at START.
Return a list with two elements: the parsed string, and
the position beyond the closing double quote."
;;; quoted          = DQUOTE *QUOTED-CHAR DQUOTE
;;;
;;; QUOTED-CHAR     = <any TEXT-CHAR except quoted-specials> /
;;;                   "\" quoted-specials
;;;
;;; quoted-specials = DQUOTE / "\"
  ;; Move past opening double quote.
  (cl-incf start)
  (let ((i start)
	string-parts)
    (while (and (< i (length line))
		(setq i
		      (cl-position-if
		       (lambda (c) (memq c '(?\" ?\\)))
		       line :start i))
		(not (eq (aref line i) ?\")))
      ;; We found a backslash.  It escapes the following character.
      (push (substring line start i) string-parts)
      (cl-incf i)
      (push (string (aref line i)) string-parts)
      (cl-incf i)
      (setf start i))
    (unless i
      (error "IMAP string not terminated"))
    (push (substring line start i) string-parts)
    (list (apply #'concat (nreverse string-parts))
	  (1+ i))))

(defun bic-quote-string (string)
  "Return STRING as an IMAP quoted string.
The return value includes the surrounding double quotes."
  (let ((acc (list "\""))
	(i 0))
    (while (and i (< i (length string)))
      (let* ((to-escape (cl-position-if
			 (lambda (c) (memq c '(?\" ?\\ ?\r ?\n)))
			 string :start i))
	     (char (when to-escape (aref string to-escape))))
	(when (memq char '(?\r ?\n))
	  ;; You should have sent a literal instead.
	  (error "Cannot send CRLF as quoted string"))
	(push (substring string i to-escape) acc)
	(if (null char)
	    (setq i nil)
	  (push (string ?\\ char) acc)
	  (setq i (1+ to-escape)))))
    (push "\"" acc)
    (apply #'concat (nreverse acc))))

(defun bic-expand-literals (sexp)
  "Replace marker pairs with strings in output from `bic--parse-line'.
Markers are set to point nowhere afterwards.  Modifies SEXP
destructively, and returns it."
  (pcase sexp
    (`(,(and start-marker (pred markerp))
       . ,(and end-marker (pred markerp)))
     (unless (and (marker-position start-marker)
		  (marker-position end-marker))
       (error "Marker already cleared"))
     (with-current-buffer (marker-buffer start-marker)
       (prog1
	   (buffer-substring start-marker end-marker)
	 (set-marker start-marker nil)
	 (set-marker end-marker nil))))
    ((pred consp)
     (setf (car sexp) (bic-expand-literals (car sexp)))
     (setf (cdr sexp) (bic-expand-literals (cdr sexp)))
     sexp)
    ((pred markerp)
     ;; This should have been caught in the first case.
     (error "Unexpected marker"))
    ((pred atom)
     sexp)
    (_
     (error "Cannot expand literals in %S" sexp))))

(defun bic-number-to-string (number)
  "Like `number-to-string', but always treats NUMBER as an integer.
If NUMBER is a float, it is truncated to the integer closest to 0.
This works correctly even if NUMBER is outside Emacs' integer range."
  (if (integerp number)
      (number-to-string number)
    (let ((float-string (number-to-string number)))
      (substring float-string 0 (cl-position ?. float-string)))))

(defun bic-format-ranges (ranges)
  "Format RANGES as a sequence-set.
RANGES is either a single cons, (START . END), or a list
where each element is either a number or a (START . END) cons.
This is the type of value returned by `gnus-compress-sequence'.

All numbers may be either integers or floats.  They will be
formatted as integers."
  (pcase ranges
    (`(,(and (pred numberp) start) . ,(and (pred numberp) end))
     (concat (bic-number-to-string start) ":" (bic-number-to-string end)))
    (_
     (let (parts)
       (dolist (range-or-number ranges)
	 (pcase range-or-number
	   (`(,(and (pred numberp) start) . ,(and (pred numberp) end))
	    (push (concat (bic-number-to-string start) ":" (bic-number-to-string end)) parts))
	   ((pred numberp)
	    (push (bic-number-to-string range-or-number) parts))
	   (_
	    (error "Invalid number or range: %S" range-or-number))))
       (mapconcat 'identity (nreverse parts) ",")))))

(defun bic-parse-sequence-set (sequence-set-string)
  "Parse a sequence set into ranges.
Does not handle sequence sets including \"*\"."
  (let ((start 0) (i 0)
	(ranges nil))
    (cl-flet
	((to-range
	  (seq-number-or-range)
	  (cond
	   ((string-match "^[0-9]+$" seq-number-or-range)
	    (let ((n (string-to-number seq-number-or-range)))
	      (cons n n)))
	   ((string-match "^\\([0-9]+\\):\\([0-9]+\\)$" seq-number-or-range)
	    (let ((first-number (string-to-number (match-string 1 seq-number-or-range)))
		  (second-number (string-to-number (match-string 2 seq-number-or-range))))
	      ;; The range endpoints can come in any order.
	      (cons (min first-number second-number)
		    (max first-number second-number))))
	   (t
	    (error "Invalid seq-number-or-range: %S" seq-number-or-range)))))
      (while (and (< i (length sequence-set-string))
		  (setq i (cl-position ?, sequence-set-string :start i)))
	;; We found a comma.
	(setq ranges (push (to-range (substring sequence-set-string start i)) ranges))
	(cl-incf i)
	(setf start i))
      (setq ranges (push (to-range (substring sequence-set-string start)) ranges)))
    ;; Everything parsed.  However, the server is allowed to return
    ;; the ranges in any order, and with overlaps.  Let's
    ;; canonicalise.

    ;; First sort by the start of each range.
    (setq ranges (cl-sort ranges #'< :key #'car))
    ;; Then check for overlap.
    (let ((ranges-without-overlap nil)
	  (pointer ranges))
      (while (cdr pointer)
	(if (and (< (caar pointer) (cl-caadr pointer))
		 (< (cdar pointer) (cl-cdadr pointer)))
	    (setq pointer (cdr pointer))
	  (let ((start-of-new-range (cdr pointer)))
	    (setf (cdr pointer) nil)
	    (setq ranges-without-overlap
		  (gnus-range-add ranges-without-overlap ranges))
	    (setq ranges start-of-new-range)
	    (setq pointer ranges))))
      (setq ranges-without-overlap
	    (gnus-range-add ranges-without-overlap ranges))
      ranges-without-overlap)))

(defun bic-connection--has-capability (capability connection)
  "Return true if CONNECTION reported CAPABILITY.
Authentication methods cannot be queried."
  (and (member capability (plist-get (fsm-get-state-data connection)
				     :capabilities))
       (not (member capability bic-ignored-capabilities))))

(provide 'bic-core)
;;; bic-core.el ends here
