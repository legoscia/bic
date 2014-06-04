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

(defvar bic-backlog-days 30
  "Messages no more than this old will be fetched.")

(defun bic (username server)
  (interactive "sIMAP username: \nsIMAP server: ")
  (push (start-bic-account username server) bic-active-accounts))

(defun bic--find-account (account)
  "Find active FSM for ACCOUNT.
ACCOUNT is a string of the form \"username@server\"."
  (or (string-match "\\(.*\\)@\\(.*\\)" account)
      (error "Invalid account name `%s'" account))
  (let ((username (match-string 1 account))
	(server (match-string 2 account)))
    (cl-find-if (lambda (state-data)
		  (and (string= username (plist-get state-data :username))
		       (string= server (plist-get state-data :server))))
		bic-active-accounts
		:key #'fsm-get-state-data)))

(define-state-machine bic-account
  :start ((username server)
	  "Start using an IMAP account."
	  (let* ((dir (expand-file-name
		       (concat username "@" server)
		       bic-data-directory))
		 (state-data (list
			      :username username
			      :server server
			      :dir dir
			      :flags-per-mailbox (make-hash-table :test 'equal))))
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
		     (concat "UID SEARCH OR UNSEEN SINCE "
			     (bic--date-text
			      (time-subtract (current-time)
					     (days-to-time bic-backlog-days))))
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
	    (let* ((count (length search-results))
		   (progress
		    (make-progress-reporter
		     (format "Fetching %d messages from %s..."
			     count selected-mailbox)
		     0 count))
		   (n 0))
	      ;; These should be UIDs, since they are a response to a UID
	      ;; SEARCH command.
	      ;; XXX: use plain search and plain fetch instead?
	      (bic-command
	       (plist-get state-data :connection)
	       (concat "UID FETCH "
		       (mapconcat #'identity search-results ",")
		       ;; TODO: Is "BODY.PEEK[]" the right choice?
		       " (ENVELOPE INTERNALDATE FLAGS BODY.PEEK[])")
	       ;; TODO: handle fetch responses one after another
	       (lambda (fetch-response)
		 (progress-reporter-done progress)
		 (fsm-send fsm (list :fetch-response
				     selected-mailbox
				     fetch-response
				     uidvalidity)))
	       (list
		(list 1 "FETCH"
		      (lambda (one-fetch-response)
			(cl-incf n)
			(progress-reporter-update progress n)
			(fsm-send fsm (list :early-fetch-response
					    selected-mailbox
					    one-fetch-response
					    uidvalidity))))))))
	  (list :connected state-data nil))
	;; TODO: handle SEARCH error
	)))
    (`(:early-fetch-response ,selected-mailbox ,msg ,uidvalidity)
     (let* ((dir (bic--mailbox-dir state-data selected-mailbox))
	    (overview-file (expand-file-name "overview" dir))
	    (flags-table (gethash selected-mailbox
				  (plist-get state-data :flags-per-mailbox)))
	    (coding-system-for-write 'binary))
       (pcase msg
	 (`(,_seq "FETCH" ,msg-att)
	  ;; TODO: check for mailbox view
	  (let* ((uid-entry (member "UID" msg-att))
		 (uid (cadr uid-entry))
		 (body-entry (member "BODY" msg-att))
		 (envelope-entry (member "ENVELOPE" msg-att))
		 (flags-entry (member "FLAGS" msg-att)))
	    (pcase body-entry
	      ((guard (null uid))
	       (warn "Missing UID in FETCH response: %S" msg))
	      (`("BODY" nil (,start-marker . ,end-marker) . ,_)
	       ;; If we do more clever fetching at some point, we'd
	       ;; have a non-nil section-spec.
	       (with-current-buffer (marker-buffer start-marker)
		 (write-region
		  start-marker end-marker
		  (expand-file-name (concat uidvalidity "-" uid) dir)
		  nil :silent))
	       ;; TODO: avoid duplicates
	       (with-temp-buffer
		 (insert uidvalidity "-" uid " ")
		 ;; TODO: better format?
		 (prin1 (bic--expand-literals (cadr envelope-entry))
			(current-buffer))
		 (insert "\n")
		 (write-region (point-min) (point-max)
			       overview-file :append :silent))
	       (puthash (concat uidvalidity "-" uid)
			(cadr flags-entry)
			flags-table))
	      (`("BODY" . ,other)
	       (message "Unexpected BODY in FETCH response: %S" other))
	      (other
	       (message "Missing BODY in FETCH response: %S" other)))))
	 (other
	  (message "Unexpected response to FETCH request: %S" other)))
       (list :connected state-data)))
    (`(:fetch-response ,selected-mailbox ,fetch-response ,uidvalidity)
     (pcase fetch-response
       (`(:ok ,_ ,fetched-messages)
	(when fetched-messages
	  (message "Extra response lines for FETCH: %S" fetched-messages)))
       (other
	(message "FETCH request failed: %S" other)))
     ;; TODO: we have the data, do something with it.
     (list :connected state-data))
    (`(:get-flags-table ,mailbox)
     (funcall callback (bic--read-flags-table state-data mailbox))
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
		  (warn "UIDVALIDITY mismatch: %s vs %s"
			(buffer-string) uidvalidity)))
	    (message "Fresh UIDVALIDITY value: %S" uidvalidity-entry)
	    (with-temp-buffer
	      (insert uidvalidity)
	      (write-region (point-min) (point-max) uidvalidity-file)))

	  (bic--read-flags-table state-data mailbox-name))
      (warn "Missing UIDVALIDITY!  This is not good.")))

  (plist-put state-data :selected mailbox-name))

(defun bic--read-flags-table (state-data mailbox-name)
  (let ((flags-table (gethash mailbox-name (plist-get state-data :flags-per-mailbox)))
	(flags-file (expand-file-name "flags" (bic--mailbox-dir state-data mailbox-name))))
    (when (null flags-table)
      (setq flags-table (make-hash-table :test 'equal))
      (puthash mailbox-name flags-table (plist-get state-data :flags-per-mailbox))

      ;; TODO: are there situations where we need to reread the flags file?
      (when (file-exists-p flags-file)
	(with-temp-buffer
	  (insert-file-contents-literally flags-file)
	  (goto-char (point-min))
	  (while (search-forward-regexp
		  (concat "^\\([0-9]+-[0-9]+\\) \\(.*\\)$")
		  nil t)
	    (let ((full-uid (match-string 1))
		  (rest (match-string 2)))
	      (pcase-let
		  ((`(,flags . ,_) (read-from-string rest)))
		(puthash full-uid flags flags-table)))))))
    flags-table))

(defun bic--date-text (time)
  (pcase-let ((`(,_sec ,_min ,_hour ,day ,month ,year . ,_)
	       (decode-time time)))
    (format "%02d-%s-%04d"
	    day
	    (aref ["Jan" "Feb" "Mar" "Apr" "May" "Jun"
		   "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]
		  (1- month))
	    year)))

;;; Mailbox view

(defvar bic--current-account nil)
(make-variable-buffer-local 'bic--current-account)

(defvar bic--current-mailbox nil)
(make-variable-buffer-local 'bic--current-mailbox)

(defvar bic--dir nil)
(make-variable-buffer-local 'bic--dir)

(defvar bic-mailbox--ewoc nil)
(make-variable-buffer-local 'bic-mailbox--ewoc)

(defvar bic-mailbox--hashtable nil)
(make-variable-buffer-local 'bic-mailbox--hashtable)

(defvar bic-mailbox--flags-table nil)
(make-variable-buffer-local 'bic-mailbox--flags-table)

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
  (let ((inhibit-read-only t)
	(buffer (current-buffer)))
    (erase-buffer)
    (setq bic-mailbox--ewoc
	  (ewoc-create #'bic-mailbox--pp))

    (let* ((uidvalidity-file (expand-file-name "uidvalidity" bic--dir))
	   (uidvalidity (with-temp-buffer
			  (insert-file-contents-literally uidvalidity-file)
			  (buffer-string)))
	   (messages (directory-files bic--dir nil
				      (concat "^" uidvalidity "-[0-9]+$"))))
      (setq bic-mailbox--hashtable (bic--read-overview uidvalidity bic--dir))
      (fsm-send (bic--find-account account) (list :get-flags-table mailbox)
		(lambda (flags-table)
		  (with-current-buffer buffer
		    (setq bic-mailbox--flags-table flags-table)
		    (let ((inhibit-read-only t))
		      (dolist (msg messages)
			(ewoc-enter-last bic-mailbox--ewoc msg)))))))))

(defun bic--read-overview (uidvalidity dir)
  (let ((overview-file (expand-file-name "overview" dir))
	(hashtable (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert-file-contents-literally overview-file)
      (goto-char (point-min))
      (while (search-forward-regexp
	      (concat "^\\(" uidvalidity "-[0-9]+\\) \\(.*\\)$")
	      nil t)
	(let ((full-uid (match-string 1))
	      (rest (match-string 2)))
	  (pcase-let
	      ((`(,envelope . ,_) (read-from-string rest)))
	    (puthash full-uid envelope hashtable)))))
    hashtable))

(defun bic-mailbox--pp (msg)
  (let ((envelope (gethash msg bic-mailbox--hashtable))
	(flags (gethash msg bic-mailbox--flags-table)))
    (pcase-let ((`(,date ,subject (,from . ,_) . ,_) envelope))
      ;; TODO: nicer format
      (insert (format "%s" flags) "\t" date "\t["
	      (if (not (string= (car from) "NIL"))
		  (car from)
		(concat (nth 2 from) "@" (nth 3 from)))
	      "]\t" subject))))

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
    (let ((inhibit-read-only t)
	  (gnus-summary-buffer (current-buffer))) ;; Don't ask :(
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
    (let ((window (display-buffer (current-buffer))))
      (set-window-start window (point-min)))))

(provide 'bic)
;;; bic.el ends here
