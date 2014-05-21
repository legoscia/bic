;;; bic.el --- Best IMAP Client                      -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Magnus Henoch

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
(require 'bic-core)
(require 'cl-lib)
(require 'utf7)
(require 'ewoc)
(require 'gnus-art)

(defvar bic-active-accounts ())

(defvar bic-data-directory (locate-user-emacs-file "bic"))

(defun bic (username server)
  (interactive "sIMAP username: \nsIMAP server: ")
  (push (start-bic-account username server) bic-active-accounts))

(define-state-machine bic-account
  :start ((username server)
	  "Start using an IMAP account."
	  (let* ((dir (expand-file-name
		       (concat username "@" server)
		       bic-data-directory))
		 (state-data (list
			      :username username
			      :server server
			      :dir dir)))
	    (if (file-directory-p dir)
		(list :existing state-data)
	      (list :no-data state-data)))))

(define-enter-state bic-account nil
  (fsm state-data)
  (setq bic-active-accounts (delq fsm bic-active-accounts))
  (list state-data nil))

(define-enter-state bic-account :no-data
  (fsm state-data)
  ;; We know nothing about this account.  Let's try connecting.
  (let ((connection
	 (start-bic-connection
	  (plist-get state-data :username)
	  (plist-get state-data :server)
	  :starttls
	  nil
	  (lambda (connection status)
	    (fsm-send fsm (list status connection))))))
    (plist-put state-data :connection connection)
    (list state-data nil)))

(define-state bic-account :no-data
  (fsm state-data event callback)
  (pcase event
    (`((:disconnected ,keyword ,reason) ,_)
     (message "Initial connection to %s@%s failed: %s (%s)"
	      (plist-get state-data :username)
	      (plist-get state-data :server)
	      reason
	      keyword)
     (list nil state-data))
    (`(:authenticated ,_)
     ;; That's good enough for now; let's create the directory.
     ;; Mail directories should not be group- or world-readable.
     (with-file-modes #o700
       (make-directory (plist-get state-data :dir) t))
     (list :connected state-data))))

(define-enter-state bic-account :existing
  (fsm state-data)
  ;; Let's try connecting, but we do have some offline data, and can
  ;; use that if we can't connect.
  (let ((connection
	 (start-bic-connection
	  (plist-get state-data :username)
	  (plist-get state-data :server)
	  :starttls
	  nil
	  (lambda (connection status)
	    (fsm-send fsm (list status connection))))))
    (plist-put state-data :connection connection)
    (list state-data nil)))

(define-state bic-account :existing
  (fsm state-data event _callback)
  (pcase event
    (`((:disconnected ,keyword ,reason) ,_)
     ;; TODO: start offline operation
     (message "Initial connection to %s@%s failed: %s (%s)"
	      (plist-get state-data :username)
	      (plist-get state-data :server)
	      reason
	      keyword)
     (list nil state-data))
    (`(:authenticated ,_)
     (list :connected state-data))))

(define-enter-state bic-account :connected
  (fsm state-data)
  ;; Get list of mailboxes
  (bic-command (plist-get state-data :connection)
	       "LIST \"\" \"*\""
	       (lambda (response)
		 (fsm-send fsm (list :list-response response))))
  (list state-data nil))

(define-state bic-account :connected
  (fsm state-data event callback)
  (pcase event
    (`(:list-response ,list-response)
     (message "Got LIST response: %S" list-response)
     (pcase list-response
       (`(:ok ,_ ,list-data)
	(bic--handle-list-response state-data list-data)
	;; TODO: open extra connection(s), start downloading interesting
	;; messages from interesting mailboxes

	;; XXX: do we _really_ want an extra connection?..  We could
	;; probably do that later.
	(let ((c (plist-get state-data :connection)))
	  ;; TODO: which mailbox?
	  (bic-command
	   c "SELECT INBOX"
	   (lambda (select-response)
	     (fsm-send fsm (list :select-response "INBOX" select-response)))))
	(list :connected state-data nil))
       ;; TODO: handle LIST error
       ))
    (`(:select-response ,mailbox-name ,select-response)
     (pcase select-response
       (`(:ok ,_ ,select-data)
	(message "Got successful SELECT response: %S" select-data)
	(bic--handle-select-response state-data mailbox-name select-data)

	(bic-command (plist-get state-data :connection)
		     ;; XXX: SEARCH or UID SEARCH?
		     "UID SEARCH UNSEEN"
		     (lambda (search-response)
		       (fsm-send fsm (list :search-response search-response))))
	(list :connected state-data nil))
       ;; TODO: handle SELECT error
       ))
    (`(:search-response ,search-response)
     (message "Got SEARCH response: %S" search-response)
     ;; TODO: download the messages in question
     (pcase search-response
       (`(:ok ,_ ,search-data)
	(let* ((search-results (cdr (assoc "SEARCH" search-data)))
	       (selected-mailbox (plist-get state-data :selected))
	       (uidvalidity
		(plist-get (cdr (assoc selected-mailbox
				       (plist-get state-data :mailboxes)))
			   :uidvalidity)))
	  (when search-results
	    ;; These should be UIDs, since they are a response to a UID
	    ;; SEARCH command.
	    (bic-command (plist-get state-data :connection)
			 (concat "UID FETCH "
				 (mapconcat #'identity search-results ",")
				 ;; TODO: Is "BODY.PEEK[]" the right choice?
				 " BODY.PEEK[]")
			 ;; TODO: handle fetch responses one after another
			 (lambda (fetch-response)
			   (fsm-send fsm (list :fetch-response
					       selected-mailbox
					       fetch-response
					       uidvalidity)))
			 (list
			  (list 1 "FETCH"
				(lambda (one-fetch-response)
				  (fsm-send fsm (list :early-fetch-response
						      selected-mailbox
						      one-fetch-response
						      uidvalidity)))))))
	  (list :connected state-data nil))
	;; TODO: handle SEARCH error
	)))
    (`(:early-fetch-response ,selected-mailbox ,msg ,uidvalidity)
     (let ((dir (bic--mailbox-dir state-data selected-mailbox))
	   (coding-system-for-write 'binary))
       (pcase msg
	 (`(,_seq "FETCH" ,msg-att)
	  (pcase (member "BODY" msg-att)
	    ((and
	      `("BODY" nil (,start-marker . ,end-marker) . ,_)
	      (let `("UID" ,uid . ,_) (member "UID" msg-att)))
	     ;; If we do more clever fetching at some point, we'd
	     ;; have a non-nil section-spec.
	     (with-current-buffer (marker-buffer start-marker)
	       (write-region
		start-marker end-marker
		(expand-file-name (concat uidvalidity "-" uid) dir)
		nil 'silent)))
	    (`("BODY" . ,other)
	     (message "Unexpected BODY in FETCH response: %S" other))
	    (other
	     (message "Missing BODY in FETCH response: %S" other))))
	 (other
	  (message "Unexpected response to FETCH request: %S" other))))
     (list :connected state-data))
    (`(:fetch-response ,selected-mailbox ,fetch-response ,uidvalidity)
     (pcase fetch-response
       (`(:ok ,_ ,fetched-messages)
	(when fetched-messages
	  (message "Extra response lines for FETCH: %S" fetched-messages)))
       (other
	(message "FETCH request failed: %S" other)))
     ;; TODO: we have the data, do something with it.
     (list :connected state-data))
    (`((:disconnected ,keyword ,reason) ,connection)
     (cond
      ((eq connection (plist-get state-data :connection))
       (message "Disconnected from IMAP server!")
       (plist-put state-data :connection nil)
       ;; TODO: check if we have enough data for offline operation.
       (list :disconnected state-data))
      (t
       ;; Not ours.
       (list :connected state-data))))))

(defconst bic-filename-invalid-characters '(?< ?> ?: ?\" ?/ ?\\ ?| ?? ?* ?_)
  "Characters not allowed inside a file name component.
This is different from `file-name-invalid-regexp', which checks
an entire file name.
It also includes underscore, which is used as an escape character.")

(defun bic--sanitize-mailbox-name (mailbox-name)
  "Convert mailbox name to directory name."
  (let ((decoded (utf7-decode mailbox-name t))
	(sanitized-to 0)
	i sanitized-segments)
    (while (setq i (cl-position-if
		    (lambda (c) (memq c bic-filename-invalid-characters))
		    decoded
		    :start sanitized-to))
      (push (substring decoded sanitized-to i) sanitized-segments)
      (push (format "_%x_" (aref decoded i)) sanitized-segments)
      (setq sanitized-to (1+ i)))
    (push (substring decoded sanitized-to) sanitized-segments)
    (apply #'concat (nreverse sanitized-segments))))

(defun bic--mailbox-dir (state-data mailbox-name)
  (expand-file-name
   (bic--sanitize-mailbox-name mailbox-name)
   (plist-get state-data :dir)))

(defun bic--handle-list-response (state-data list-data)
  (let ((mailboxes
	 (mapcar
	  ;; TODO: What about \Noselect?
	  (lambda (x)
	    (pcase x
	      (`("LIST" ,_ ,_ ,mailbox-name)
	       mailbox-name)
	      (_
	       (error "Unexpected LIST response: %S" x))))
	  list-data)))
    (mapc
     (lambda (mailbox-name)
       (make-directory
	(bic--mailbox-dir state-data mailbox-name)
	t))
     mailboxes)
    ;; Modify state-data in place:
    (plist-put state-data :mailboxes (mapcar #'list mailboxes))))

(defun bic--handle-select-response (state-data mailbox-name select-data)
  (let ((uidvalidity-entry
	 (cl-find-if
	  (lambda (x)
	    (and (eq (car-safe x) :ok)
		 (string= "UIDVALIDITY"
			  (plist-get (cdr x) :code))))
	  select-data)))
    (if uidvalidity-entry
	(let ((uidvalidity-file
	       (expand-file-name
		"uidvalidity"
		(bic--mailbox-dir state-data mailbox-name)))
	      (uidvalidity (plist-get (cdr uidvalidity-entry) :data))
	      (mailbox-entry (assoc mailbox-name (plist-get state-data :mailboxes))))
	  (if (null mailbox-entry)
	      (warn "Selecting unknown mailbox `%s'" mailbox-name)
	    (setf (cdr mailbox-entry)
		  (plist-put (cdr mailbox-entry) :uidvalidity uidvalidity)))
	  (if (file-exists-p uidvalidity-file)
	      (with-temp-buffer
		(insert-file-contents-literally uidvalidity-file)
		(if (string= (buffer-string) uidvalidity)
		    (message "UIDVALIDITY match")
		  ;; TODO
		  (message "UIDVALIDITY mismatch: %s vs %s"
			   (buffer-string) uidvalidity)))
	    (message "Fresh UIDVALIDITY value: %S" uidvalidity-entry)
	    (with-temp-buffer
	      (insert uidvalidity)
	      (write-region (point-min) (point-max) uidvalidity-file))))
      (message "Missing UIDVALIDITY!  This is not good.")))
  (plist-put state-data :selected mailbox-name))

;;; Mailbox view

(defvar bic--current-account nil)
(make-variable-buffer-local 'bic--current-account)

(defvar bic--current-mailbox nil)
(make-variable-buffer-local 'bic--current-mailbox)

(defvar bic--dir nil)
(make-variable-buffer-local 'bic--dir)

(defvar bic-mailbox--ewoc nil)
(make-variable-buffer-local 'bic-mailbox--ewoc)

(defun bic-mailbox-open (account mailbox)
  (interactive
   (let* ((accounts (directory-files bic-data-directory nil "@"))
	  (account (completing-read "IMAP account: " accounts))
	  (mailboxes (directory-files (expand-file-name account bic-data-directory) nil "[^.]"))
	  ;; XXX: unsanitize name
	  (mailbox (completing-read "Mailbox: " mailboxes)))
     (list account mailbox)))
  (let ((buffer-name (concat mailbox "-" account)))
    (with-current-buffer (get-buffer-create buffer-name)
      (unless (derived-mode-p 'bic-mailbox-mode)
	(bic-mailbox-mode)
	(bic-mailbox--init account mailbox)))
    (switch-to-buffer buffer-name)))

(defvar bic-mailbox-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") 'bic-mailbox-read-message)
    map))

(define-derived-mode bic-mailbox-mode special-mode "BIC mailbox"
  "Major mode for IMAP mailboxes accessed by `bic'."
  (setq header-line-format
	'(" " bic--current-account " " bic--mailbox--current-mailbox)))

(defun bic-mailbox--init (account mailbox)
  (setq bic--current-account account
	bic--current-mailbox mailbox
	bic--dir (expand-file-name
		  mailbox
		  (expand-file-name
		   account bic-data-directory)))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq bic-mailbox--ewoc
	  (ewoc-create #'bic-mailbox--pp))

    (let* ((uidvalidity-file (expand-file-name "uidvalidity" bic--dir))
	   (uidvalidity (with-temp-buffer
			  (insert-file-contents-literally uidvalidity-file)
			  (buffer-string)))
	   (messages (directory-files bic--dir nil
				      (concat "^" uidvalidity "-[0-9]+$"))))
      (dolist (msg messages)
	(ewoc-enter-last bic-mailbox--ewoc msg)))))

(defun bic-mailbox--pp (msg)
  ;; TODO: display sender, subject etc
  (insert msg))

(defun bic-mailbox-read-message ()
  "Open the message under point."
  (interactive)
  (unless (derived-mode-p 'bic-mailbox-mode)
    (user-error "Not a mailbox buffer"))
  (let ((msg (ewoc-data (ewoc-locate bic-mailbox--ewoc (point)))))
    (bic-message-display bic--current-account
			 bic--current-mailbox
			 msg)))

;;; Message view

(define-derived-mode bic-message-mode gnus-article-mode "BIC Message"
  "Major mode for messages viewed from `bic'.")

(defun bic-message-display (account mailbox msg)
  (with-current-buffer (get-buffer-create "*BIC-Message*")
    (let ((inhibit-read-only t))
      (bic-message-mode)
      (setq bic--current-account account
	    bic--current-mailbox mailbox
	    bic--dir (expand-file-name
		      mailbox
		      (expand-file-name
		       account bic-data-directory)))
      ;; XXX: ideally we should use insert-file-contents-literally
      ;; here, but gnus-article-mode gets very confused by our CRLF
      ;; line endings.
      (insert-file-contents (expand-file-name msg bic--dir)
			    nil nil nil t)
      ;; Gnus already does a fine job displaying messages, so we might
      ;; as well piggy-back on that:
      (gnus-article-prepare-display))
    (display-buffer (current-buffer))))

(provide 'bic)
;;; bic.el ends here
