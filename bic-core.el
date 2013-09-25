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

(defvar bic-transcript-buffer "*bic-transcript-%s*")

(defvar bic-ignore-tls-errors nil
  "If non-nil, ignore certificate verification errors.")

(define-state-machine bic-connection
  :start ((username server)
	  "Start an IMAP connection."
	  (list :connecting
		(list :name (concat username "@" server)
		      :username username
		      :server server))))

(define-enter-state bic-connection :connecting
  (fsm state-data)
  (let* ((server (plist-get state-data :server))
	 (proc (make-network-process
		:name (concat "bic-" server)
		:buffer (generate-new-buffer (concat "bic-" server))
		:host server
		:service 143
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
       (list :wait-for-greeting state-data nil))
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
   ((not (plist-get state-data :encrypted))
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
      (if (null mechanism)
	  (progn
	    (message "No suitable mechanism found!  We support %s, server supports %s"
		     sasl-mechanisms server-mechanisms)
	    (list nil nil nil))
	(let* ((client
		(sasl-make-client
		 mechanism
		 (plist-get state-data :username)
		 "imap"
		 (plist-get state-data :server)))
	       (step (sasl-next-step client nil)))
	  (bic--send fsm (concat "auth AUTHENTICATE " (sasl-mechanism-name mechanism) "\r\n"))
	  (list :sasl-auth (plist-put
			    (plist-put state-data :sasl-client client)
			    :sasl-step step))))))))

(define-state bic-connection :wait-for-starttls-response
  (fsm state-data event callback)
  (pcase event
    (`(:filter ,process ,data)
     (bic--filter process data fsm)
     (list :wait-for-starttls-response state-data))
    (`(:line ,line)
     (cond
      ((string-prefix-p "foo OK " line)
       (gnutls-negotiate :process (plist-get state-data :proc)
			 :hostname (plist-get state-data :server)
			 :verify-hostname-error (not bic-ignore-tls-errors)
			 :verify-error (not bic-ignore-tls-errors))
       ;; No error?  Connection encrypted!
       (message "STARTTLS negotiated")
       ;; Forget capabilities and ask again on encrypted connection.
       (list :wait-for-capabilities
	     (plist-put
	      (plist-put state-data :encrypted t)
	      :capabilities nil)))
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

(define-state bic-connection :sasl-auth
  (fsm state-data event callback)
  (pcase event
    (`(:filter ,process ,data)
     (bic--filter process data fsm)
     (list :sasl-auth state-data))
    (`(:line ,line)
     (cond
      ((string-prefix-p "+ " line)
       (let ((data (substring line 2))
	     (client (plist-get state-data :sasl-client))
	     (step (plist-get state-data :sasl-step)))
	 (sasl-step-set-data step (base64-decode-string data))
	 (setq step (sasl-next-step client step))
	 (bic--send fsm (concat (base64-encode-string (or (sasl-step-data step) "") t) "\r\n")
		    :sensitive t)
	 ;; XXX: check local success/failure, for mechanisms that
	 ;; simultaneously authenticate the server
	 (list :sasl-auth (plist-put state-data :sasl-step step))))
      ((string-prefix-p "auth " line)
       (pcase (bic--parse-line line)
	 (`(,_ "OK" ,message)
	  ;; XXX: check local success/failure here too
	  (message "IMAP authentication successful: %s" message)
	  (list :authenticated state-data))
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

(defun bic--filter (process data fsm)
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert data)

    ;; Find complete lines, terminated by CRLF
    (goto-char (point-min))
    (while (search-forward "\r\n" nil t)
      (let* ((line-crlf (delete-and-extract-region (point-min) (match-end 0)))
	     (line (substring line-crlf 0 -2)))
	;; TODO: filter sensitive data
	(bic--transcript fsm (concat "S: " line "\n"))
	;; Send the line as an event to the FSM
	(fsm-send fsm (list :line line))))))

(cl-defun bic--send (fsm string &key sensitive)
  ;; XXX: too many newlines?
  (bic--transcript fsm (concat "C: " (if sensitive "<omitted>" string) "\n"))
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
    (error "Unexpected greeting: %s" line0))))

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
