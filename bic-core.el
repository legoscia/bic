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

(require 'fsm)
(require 'sasl)
(require 'gnutls)

(defvar bic-transcript-buffer "*bic-transcript-%s*")

(defvar bic-ignore-tls-errors nil
  "If non-nil, ignore certificate verification errors.")

(defvar bic-send-cleartext-password nil
  "If non-nil, allow sending passwords on unencrypted connections.")

(define-state-machine bic-connection
  :start ((username server connection-type)
	  "Start an IMAP connection."
	  (list :connecting
		(list :name (concat username "@" server)
		      :username username
		      :server server
		      :connection-type connection-type))))

(define-enter-state bic-connection :connecting
  (fsm state-data)
  (let* ((server (plist-get state-data :server))
	 (connection-type (plist-get state-data :connection-type))
	 (proc (make-network-process
		:name (concat "bic-" server)
		:buffer (generate-new-buffer (concat "bic-" server))
		:host server
		:service (ecase connection-type
			   ((:starttls :unencrypted) 143)
			   (:plaintls 993))
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
       (ecase (plist-get state-data :connection-type)
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
	     (message "Cannot negotiate TLS for %s: %s"
		      (plist-get state-data :server)
		      (error-message-string e))
	     (list nil nil nil))))))
      ((string-prefix-p "failed" string)
       ;; XXX: handle gracefully
       (message "connection failed: %s" string)
       (list nil nil nil))
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
	(message "Server wants to disconnect: %s" text)
	(list nil nil nil))))
    (event
     (message "Got event %S" event)
     (list :wait-for-greeting state-data))))

(define-enter-state bic-connection :wait-for-capabilities
  (fsm state-data)
  (bic--send fsm "foo CAPABILITY\r\n")
  (list state-data nil))

(define-state bic-connection :wait-for-capabilities
  (fsm state-data event callback)
  (pcase event
    (`(:filter ,process ,data)
     (bic--filter process data fsm)
     (list :wait-for-capabilities state-data))
    (`(:line ,line)
     (pcase (bic--parse-line line)
       (`("*" "CAPABILITY" ,capabilities-string)
	(let ((capabilities (bic--parse-capabilities capabilities-string)))
	  (list :wait-for-capabilities
		(plist-put state-data :capabilities capabilities))))
       (`("foo" "OK" ,_)
	(bic--advance-connection-state
	 fsm state-data
	 (plist-get state-data :capabilities)))
       (_
	(message "Unexpected: %s" line)
	(list nil nil nil))))
    (event
     (message "Got event %S" event)
     (list :wait-for-capabilities state-data))))

(defun bic--advance-connection-state (fsm state-data capabilities)
  (cond
   ((and (not (plist-get state-data :encrypted))
	 (not (eq (plist-get state-data :connection-type) :unencrypted)))
    (if (member "STARTTLS" capabilities)
	(progn
	  (send-string (plist-get state-data :proc)
		       "foo STARTTLS\r\n")
	  (list :wait-for-starttls-response state-data))
      ;; TODO
      (message "STARTTLS not available!")
      (list nil nil nil)))
   ((not (plist-get state-data :authenticated))
    (let* ((server-mechanisms (cdr (assq :auth capabilities)))
	   (mechanism (sasl-find-mechanism server-mechanisms)))
      (cond
       ((null mechanism)
	(message "No suitable mechanism found!  We support %s, server supports %s"
		 sasl-mechanisms server-mechanisms)
	(list nil nil nil))
       ((and (not bic-send-cleartext-password)
	     (not (plist-get state-data :encrypted))
	     (member (sasl-mechanism-name mechanism) '("PLAIN" "LOGIN")))
	(message "Cleartext authentication not allowed!  Server offers %s"
		 server-mechanisms)
	(list nil nil nil))
       (t
	(let* ((client
		(sasl-make-client
		 mechanism
		 (plist-get state-data :username)
		 "imap"
		 (plist-get state-data :server)))
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
    (list :authenticated))))

(define-state bic-connection :wait-for-starttls-response
  (fsm state-data event callback)
  (pcase event
    (`(:filter ,process ,data)
     (bic--filter process data fsm)
     (list :wait-for-starttls-response state-data))
    (`(:line ,line)
     (cond
      ((string-prefix-p "foo OK " line)
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
	  (message "Cannot negotiate STARTTLS for %s: %s"
		   (plist-get state-data :server)
		   (error-message-string e))
	  (list nil nil nil))))
      ((string-prefix-p "foo BAD " line)
       (message "Cannot negotiate STARTTLS: %s"
		(substring line 8))
       (list nil nil nil))
      (t
       (message "Unexpected response to STARTTTLS command: %s" line)
       (list nil nil nil))
      (event
       (message "Got event %S" event)
       (list :wait-for-starttls-response state-data))))))

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
     (cond
      ((string-prefix-p "+ " line)
       (let ((data (substring line 2))
	     (client (plist-get state-data :sasl-client))
	     (step (plist-get state-data :sasl-step)))
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
      ((string-prefix-p "auth " line)
       (pcase (bic--parse-line line)
	 (`(,_ "OK" ,resp-text)
	  ;; XXX: check local success/failure here too
	  (destructuring-bind (new-capabilities . text)
	      (if (string-match "^\\[CAPABILITY \\([^]]*\\)\\] \\(.*\\)" resp-text)
		  (cons (save-match-data
			  (bic--parse-capabilities (match-string 1 resp-text)))
			(match-string 2 resp-text))
		(cons nil resp-text))
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
	 (`(,_ "NO" ,message)
	  (message "IMAP authentication failed: %s" message)
	  ;; TODO: ask for better password?
	  (list nil nil nil))
	 (`(,_ "BAD" ,message)
	  ;; This shouldn't happen
	  (message "Unexpected IMAP authentication error: %s" message)
	  (list nil nil nil))))
      (t
       (message "Unexpected input: %s" line)
       (list nil nil nil))))
    (event
     (message "Got event %S" event)
     (list :sasl-auth state-data))))

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
       (`("*" ,type ,rest)
	;; TODO: maybe send results early
	(plist-put state-data :response-acc
		   (cons (cons type rest) (plist-get state-data :response-acc))))
       (`(,tag ,type ,rest)
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
     (list :authenticated state-data))))

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

(defun bic--negotiate-tls (state-data)
  (gnutls-negotiate :process (plist-get state-data :proc)
		    :hostname (plist-get state-data :server)
		    :verify-hostname-error (not bic-ignore-tls-errors)
		    :verify-error (not bic-ignore-tls-errors)))

(cl-defun bic--filter (process data fsm &key sensitive)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert data)

    ;; Find complete lines, terminated by CRLF
    (goto-char (point-min))
    (while (search-forward "\r\n" nil t)
      (let* ((line-crlf (delete-and-extract-region (point-min) (match-end 0)))
	     (line (substring line-crlf 0 -2)))
	(bic--transcript fsm (concat "S: "
				     (if (and sensitive (funcall sensitive line))
					 "<omitted>"
				       line)
				     "\n"))
	;; Send the line as an event to the FSM
	(fsm-send fsm (list :line line))))))

(cl-defun bic--send (fsm string &key sensitive)
  ;; XXX: too many newlines?
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
  (cond
   ((string-prefix-p "* BYE " line)
    (list :bye (substring line 6)))
   ((string-prefix-p "* OK " line)
    (let ((resp-text (substring line 5)))
      (if (string-match "\\[CAPABILITY \\([^]]*\\)\\] \\(.*\\)" line)
	  (let ((capabilities-string (match-string 1 line))
		(text (match-string 2 line)))
	    (list :ok (bic--parse-capabilities capabilities-string) text))
	(list :ok nil resp-text))))
   ;; TODO: PREAUTH
   (t
    (error "Unexpected greeting: %s" line))))

(defun bic--parse-capabilities (string)
  (let ((start 0) capabilities auth)
    (while (string-match "[^ ]+" string start)
      (setq start (match-end 0))
      (let ((capability (match-string 0 string)))
	(if (string-prefix-p "AUTH=" capability)
	    (push (substring capability 5) auth)
	  (push capability capabilities))))
    (cons (cons :auth auth) capabilities)))

(defun bic--parse-line (line)
  (if (not (string-match "^\\([^ ]+\\) \\([^ ]+\\) \\(.*\\)$" line))
      :unexpected
    (let ((tag (match-string 1 line))
	  (type (match-string 2 line))
	  (rest (match-string 3 line)))
      (list tag type rest))))
      
(provide 'bic-core)
;;; bic-core.el ends here
