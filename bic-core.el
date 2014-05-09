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

(define-state-machine bic-connection
  :start ((username server connection-type &optional port callback)
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
connection is closed."
	  (list :connecting
		(list :name (concat username "@" server)
		      :username username
		      :server server
		      :port port
		      :connection-type connection-type
		      :callback (or callback #'ignore)))))

(define-enter-state bic-connection :connecting
  (fsm state-data)
  (let* ((server (plist-get state-data :server))
	 (connection-type (plist-get state-data :connection-type))
	 (proc (make-network-process
		:name (concat "bic-" server)
		:buffer (generate-new-buffer (concat "bic-" server))
		:host server
		:service (or (plist-get state-data :port)
			     (cl-ecase connection-type
			       ((:starttls :unencrypted) 143)
			       (:plaintls 993)))
		:coding 'binary
		:nowait t
		:filter (fsm-make-filter fsm)
		:sentinel (fsm-make-sentinel fsm))))
    (list (plist-put state-data :proc proc) nil)))

(define-state bic-connection :connecting
  (fsm state-data event callback)
  (pcase event
    (`(:sentinel ,_ ,string)
     (cond
      ((string-prefix-p "open" string)
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
      ((string-prefix-p "failed" string)
       (bic--fail state-data
		  :connection-failed
		  (format "connection failed: %s" string)))
      (t
       (message "Unknown sentinel event %S" string)
       (list :connecting state-data nil))))
    (unexpected
     (message "Unexpected event %S" unexpected)
     (list :connecting state-data nil))))

(define-state bic-connection :wait-for-greeting
  (fsm state-data event callback)
  (pcase event
    (`(:filter ,process ,data)
     (bic--filter process data fsm)
     (list :wait-for-greeting state-data))
    (`(:line ,line)
     (pcase (bic--parse-greeting line)
       (`(:ok ,capabilities ,greeting-text)
	(message "Server said '%s'" greeting-text)
	(if (null capabilities)
	    (list :wait-for-capabilities state-data)
	  (bic--advance-connection-state fsm state-data capabilities)))
       (`(:bye ,text)
	(bic--fail state-data
		   :server-disconnect
		   (format "Server wants to disconnect: %s" text)))))
    (event
     (message "Got event %S" event)
     (list :wait-for-greeting state-data))))

(define-enter-state bic-connection :wait-for-capabilities
  (fsm state-data)
  (bic--send fsm "caps CAPABILITY\r\n")
  (list state-data nil))

(define-state bic-connection :wait-for-capabilities
  (fsm state-data event callback)
  (pcase event
    (`(:filter ,process ,data)
     (bic--filter process data fsm)
     (list :wait-for-capabilities state-data))
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
    (event
     (message "Got event %S" event)
     (list :wait-for-capabilities state-data))))

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
	       (step (sasl-next-step client nil)))
	  ;; XXX: we can't send the AUTHENTICATE command here, because
	  ;; sending data over a network connection means that we can
	  ;; receive data as well, which causes a race condition
	  ;; whereby the filter function being called with the server
	  ;; response before we've moved on to the :sasl-auth state.
	  ;; Thus we send the AUTHENTICATE command in the enter
	  ;; function instead.
	  (list :sasl-auth (plist-put
			    (plist-put state-data :sasl-client client)
			    :sasl-step step)))))))
   (t
    (list :authenticated state-data))))

(define-state bic-connection :wait-for-starttls-response
  (fsm state-data event callback)
  (pcase event
    (`(:filter ,process ,data)
     (bic--filter process data fsm)
     (list :wait-for-starttls-response state-data))
    (`(:line ,line)
     (pcase (bic--parse-line line)
       (`("starttls" :ok . ,_)
	(condition-case e
	    (progn
	      (bic--negotiate-tls state-data)
	      ;; No error?  Connection encrypted!
	      (message "STARTTLS negotiated")
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
    (event
     (message "Got event %S" event)
     (list :wait-for-starttls-response state-data))))

(define-enter-state bic-connection :sasl-auth
  (fsm state-data)
  (let* ((client (plist-get state-data :sasl-client))
	 (mechanism (sasl-client-mechanism client)))
    (bic--send fsm (concat "auth AUTHENTICATE " (sasl-mechanism-name mechanism) "\r\n")))
  (list state-data nil))

(define-state bic-connection :sasl-auth
  (fsm state-data event callback)
  (pcase event
    (`(:filter ,process ,data)
     (bic--filter process data fsm :sensitive (apply-partially #'string-prefix-p "+ "))
     (list :sasl-auth state-data))
    (`(:line ,line)
     (message "Got: %S" (bic--parse-line line))
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
	  (unless (and (zerop (length data))
		       (null (plist-get state-data :sasl-sent-message)))
	    (sasl-step-set-data step (base64-decode-string data))
	    (setq step (sasl-next-step client step)))
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
	  (list :sasl-auth state-data)))
       (`("auth" :ok . ,plist)
	;; XXX: check local success/failure here too
	(let ((new-capabilities
	       (when (string= (plist-get plist :code) "CAPABILITY")
		 (bic--parse-capabilities (split-string (plist-get plist :data)))))
	      (text (plist-get plist :text)))
	  (message "IMAP authentication successful: %s" text)
	  (plist-put state-data :authenticated t)
	  (plist-put state-data :capabilities new-capabilities)
	  (plist-put state-data :sasl-client nil)
	  (plist-put state-data :sasl-step nil)
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
    (event
     (message "Got event %S" event)
     (list :sasl-auth state-data))))

(defun bic--read-passphrase-function (state-data)
  (lambda (prompt)
    (let ((found (car
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
		   :require '(:secret)))))
      ;; Copy the password, as sasl.el wants to erase it.
      (copy-sequence
       (if found
	   ;; Got it from auth-source.
	   (let ((secret (plist-get found :secret)))
	     (if (functionp secret)
		 (funcall secret)
	       secret))
	 ;; Ask the user.
	 (read-passwd prompt))))))

(define-enter-state bic-connection :authenticated
  (fsm state-data)
  (funcall (plist-get state-data :callback) fsm :authenticated)
  (list state-data nil))

(define-state bic-connection :authenticated
  (fsm state-data event callback)
  (pcase event
    (`(:cmd ,cmd)
     (let* ((tag-number (or (plist-get state-data :next-tag) 0))
	    (next-tag (1+ tag-number))
	    (tag-string (number-to-string tag-number))
	    (pending-commands
	     ;; Need to keep commands in the correct order.  We
	     ;; shouldn't have that many pending commands, so
	     ;; appending all the time should be fine.
	     (append (plist-get state-data :pending-commands)
		     (list (cons tag-string callback)))))
       (plist-put state-data :pending-commands pending-commands)
       (plist-put state-data :next-tag next-tag)
       (bic--send fsm (concat tag-string " " cmd "\r\n"))
       (list :authenticated state-data)))

    (`(:filter ,process ,data)
     (bic--filter process data fsm)
     (list :authenticated state-data))

    (`(:line ,line)
     (pcase (bic--parse-line line)
       (`("*" ,type . ,rest)
	;; TODO: maybe send results early
	(plist-put state-data :response-acc
		   (cons (cons type rest) (plist-get state-data :response-acc))))
       (`(,tag ,type . ,rest)
	(let* ((pending-commands (plist-get state-data :pending-commands))
	       (entry (assoc tag pending-commands))
	       (command-callback (cdr entry))
	       (new-pending-commands (delq entry pending-commands))
	       (response-acc (plist-get state-data :response-acc)))
	  (plist-put state-data :response-acc nil)
	  (plist-put state-data :pending-commands new-pending-commands)
	  (funcall command-callback (list type rest response-acc))))
       (_
	(fsm-debug-output "Unexpected line: '%s'" line)))
     (list :authenticated state-data))

    (`(:sentinel ,_process ,reason)
     ;; strip trailing newline
     (when (eq ?\n (aref reason (1- (length reason))))
       (setq reason (substring reason 0 -1)))
     (bic--fail state-data :connection-closed reason))))

(defun bic-command (fsm command callback)
  "Send an IMAP command.
COMMAND is a string containing an IMAP command minus the tag.
CALLBACK is a function that takes one argument of the form
\(RESPONSE TEXT RESPONSE-LINES), where RESPONSE is a string
containing the response type, typically \"OK\", \"NO\" or
\"BAD\", TEXT is the rest of the tagged response line, and
RESPONSE-LINES is a list of (TYPE TEXT) entries, one for
each untagged response line.

The callback function will be called when the command has
finished.  There is no immediate response."
  (fsm-send fsm `(:cmd ,command) callback))

(defun bic--fail (state-data keyword reason)
  (plist-put state-data :fail-keyword keyword)
  (plist-put state-data :fail-reason reason)
  (list nil state-data))

(define-enter-state bic-connection nil
  (fsm state-data)
  ;; Delete the connection just to be sure it's gone.
  (let ((proc (plist-get state-data :proc)))
    (when (processp proc)
      (delete-process proc)))
  (let ((callback (plist-get state-data :callback))
	(fail-keyword (or (plist-get state-data :fail-keyword)
			  :unknown-reason))
	(fail-reason (or (plist-get state-data :fail-reason)
			 "Unexpected error")))
    (message "IMAP connection closed: %s" fail-reason)
    (funcall callback fsm (list :disconnected fail-keyword fail-reason)))
  (list nil nil))

(define-state bic-connection nil
  (fsm state-data event callback)
  ;; Ignore all events
  (list nil state-data))

(defun bic--negotiate-tls (state-data)
  (gnutls-negotiate :process (plist-get state-data :proc)
		    :hostname (plist-get state-data :server)
		    :verify-hostname-error (not bic-ignore-tls-errors)
		    :verify-error (not bic-ignore-tls-errors)))

(defvar bic--unread-start-marker nil)
(make-variable-buffer-local 'bic--unread-start-marker)

(defvar bic--literal-start-marker nil)
(make-variable-buffer-local 'bic--literal-start-marker)

(defvar bic--literal-expected-length nil)
(make-variable-buffer-local 'bic--literal-expected-length)

(defvar bic--line-acc nil)
(make-variable-buffer-local 'bic--line-acc)

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
      (push (cons bic--literal-start-marker (copy-marker literal-end)) bic--line-acc)
      (setq bic--literal-start-marker nil)
      (set-marker bic--unread-start-marker literal-end)
      t))))

(cl-defun bic--send (fsm string &key sensitive)
  (bic--transcript fsm
		   (concat "C: "
			   (if sensitive
			       "<omitted>"
			     (if (string= (substring string -2) "\r\n")
				 (substring string 0 -2)
			       string)) "\n"))
  (send-string (plist-get (fsm-get-state-data fsm) :proc) string))

(defun bic--transcript (fsm string)
  (with-current-buffer (get-buffer-create (format bic-transcript-buffer (plist-get (fsm-get-state-data fsm) :name)))
    (save-excursion
      (goto-char (point-max))
      (insert string))))

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
  (if (string-match "\\[\\([^]\s]+\\)\\(?: \\([^]]*\\)\\)?\\] \\(.*\\)" resp-text)
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
  (let ((i (1+ start))
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

(provide 'bic-core)
;;; bic-core.el ends here
