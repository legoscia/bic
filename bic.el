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
(require 'srv)
(require 'bic-core)
(require 'cl-lib)
(require 'utf7)
(require 'ewoc)
(require 'gnus-art)
(require 'gnus-range)
(require 'hex-util)

(defgroup bic nil
  "Settings for the Best IMAP Client."
  :group 'mail)

(defvar bic-running-accounts ())

(defvar bic-data-directory (locate-user-emacs-file "bic"))

(defvar bic-backlog-days 30
  "Messages no more than this old will be fetched.")

(defvar bic-reconnect-interval 5
  "Attempt to reconnect after this many seconds.")

(defun bic (address)
  (interactive "sEmail address: ")
  (push (start-bic-account address) bic-running-accounts))

(defun bic-deactivate (account)
  (interactive (list (bic--read-running-account)))
  (fsm-send (bic--find-account account) :deactivate))

(defun bic-activate (account)
  (interactive (list (bic--read-running-account)))
  (fsm-send (bic--find-account account) :activate))

(defun bic--read-running-account ()
  "Read the name of a running account from the minibuffer."
  (if (null bic-running-accounts)
      (user-error "No accounts available")
    (completing-read "Account: "
		     (mapcar
		      (lambda (fsm)
			(plist-get (fsm-get-state-data fsm) :address))
		      bic-running-accounts)
		     nil t)))

(defun bic--find-account (account)
  "Find active FSM for ACCOUNT.
ACCOUNT is a string of the form \"username@server\"."
  (cl-find-if (lambda (state-data)
		(string= account (plist-get state-data :address)))
	      bic-running-accounts
	      :key #'fsm-get-state-data))

(define-state-machine bic-account
  :start ((address)
	  "Start using an IMAP account."
	  (let* ((dir (expand-file-name address bic-data-directory))
		 (state-data (list
			      :address address
			      :dir dir
			      :overview-per-mailbox (make-hash-table :test 'equal)
			      :flags-per-mailbox (make-hash-table :test 'equal))))
	    (if (file-directory-p dir)
		(list :existing state-data)
	      (list :no-data state-data)))))

(define-enter-state bic-account nil
  (fsm state-data)
  (setq bic-running-accounts (delq fsm bic-running-accounts))
  (list state-data nil))

(define-enter-state bic-account :no-data
  (fsm state-data)
  ;; We know nothing about this account.  Let's try to figure out the
  ;; mail server hostname.
  ;;
  ;; First, look for SRV records, as described in RFC 6186.
  ;;
  ;; We don't follow the specifications precisely when it comes to
  ;; verifying certificates for hostnames found through SRV, but
  ;; that's because noone else does.
  (let* ((address (plist-get state-data :address))
	 (domain-name (substring address (1+ (cl-position ?@ address))))
	 (imaps-srv-results (srv-lookup (concat "_imaps._tcp." domain-name)))
	 (srv-candidate nil)
	 (candidates nil))
    ;; Empty hostname means "service not available".
    (setq imaps-srv-results (cl-delete "" imaps-srv-results :key 'car))
    (if imaps-srv-results
	;; Strictly speaking, we should go through all results in
	;; priority order, and pick the first one that works, but
	;; since the hosts are unlikely to have SRV-ID (RFC 4985)
	;; fields in their certificates (and since we don't have
	;; methods to retrieve those fields), we would have to ask the
	;; user to confirm every single host, which would be
	;; confusing.  So let's pick the first one for now.
	(setq srv-candidate (list
			     (caar imaps-srv-results)
			     (cdar imaps-srv-results)
			     :plaintls))
      ;; No IMAPS service advertised.  What about plain IMAP
      ;; (hopefully with STARTTLS)?
      (let ((imap-srv-results (srv-lookup (concat "_imap._tcp." domain-name))))
	(setq imap-srv-results (cl-delete "" imap-srv-results :key 'car))
	(when imap-srv-results
	  (setq srv-candidate (list
			       (caar imap-srv-results)
			       (cdar imap-srv-results)
			       :starttls)))))

    ;; Ask user if SRV result is okay.
    (when srv-candidate
      (when (yes-or-no-p
	     (format "Use %s:%d as IMAP server? "
		     (car srv-candidate)
		     (cadr srv-candidate)))
	(setq candidates (list srv-candidate))))

    (unless candidates
      (let* ((response (read-string
			(format "IMAP server for %s (hostname or hostname:port): "
				address)))
	     (colon-pos (cl-position ?: response)))
	(if colon-pos
	    ;; User specified port
	    (let ((hostname (substring response 0 colon-pos))
		  (port (string-to-number (substring response (1+ colon-pos)))))
	      ;; For ports 143 and 993, we know what to expect.
	      (cl-case port
		(143
		 (push (list hostname port :starttls) candidates))
		(993
		 (push (list hostname port :plaintls) candidates))
		(t
		 ;; For other ports, try both plain TLS and STARTTLS.
		 (push (list hostname port :starttls) candidates)
		 (push (list hostname port :plaintls) candidates))))
	  ;; No port specified.  Try both 143 and 993.
	  (push (list response 143 :starttls) candidates)
	  (push (list response 993 :plaintls) candidates))))

    (plist-put state-data :candidates candidates)
    (let* ((address (plist-get state-data :address))
	   (user-part (substring address 0 (cl-position ?@ address))))
      (pcase (widget-choose
	      "IMAP username"
	      (list (cons (concat "Authenticate as " address) address)
		    (cons (concat "Authenticate as " user-part) user-part)
		    (cons "Use a different username" nil)))
	((and (pred stringp) username)
	 (plist-put state-data :username username))
	(`nil
	 (plist-put state-data :username (read-string "Enter IMAP username: ")))))
    (fsm-send fsm :try-connect)
    (list state-data nil)))

(define-state bic-account :no-data
  (fsm state-data event _callback)
  (pcase event
    (:try-connect
     ;; Now we have a number of connection candidates.  Let's connect
     ;; to all of them and pick the one that answers first.
     (let ((candidate-connections
	    (mapcar
	     (lambda (candidate)
	       (pcase-let ((`(,hostname ,port ,connection-type) candidate))
		 (cons
		  (start-bic-connection
		   (plist-get state-data :username)
		   hostname
		   connection-type
		   port
		   (lambda (connection status)
		     (fsm-send fsm (list status connection)))
		   :auth-wait)
		  candidate)))
	     (plist-get state-data :candidates))))
       (plist-put state-data :candidate-connections candidate-connections)
       (list :no-data state-data)))

    ((and (guard (not (plist-get state-data :connection)))
	  `(:auth-wait ,chosen-connection))
     ;; One of our candidate connections is ready to begin
     ;; authentication, and we haven't chosen a different connection
     ;; already.
     (let* ((candidate-connections (plist-get state-data :candidate-connections))
	    (chosen-candidate (cdr (assq chosen-connection candidate-connections)))
	    (other-connections
	     (remq chosen-connection (mapcar 'car candidate-connections))))
       (plist-put state-data :chosen-candidate chosen-candidate)
       ;; TODO: close `other-connections'
       (plist-put state-data :connection chosen-connection)
       (fsm-send chosen-connection :proceed)
       (list :no-data state-data)))

    (`((:disconnected ,keyword ,reason) ,bad-connection)
     (let* ((candidate-connections (plist-get state-data :candidate-connections))
	    (bad-candidate (cdr (assq bad-connection candidate-connections))))
       (message "Connection to %s:%d failed: %s (%s)"
		(cl-first bad-candidate) (cl-second bad-candidate)
		reason keyword)
       (setq candidate-connections
	     (cl-remove bad-connection candidate-connections :key 'car))
       (plist-put state-data :candidate-connections candidate-connections)
       (if candidate-connections
	   ;; We are still waiting to hear from some other connection.
	   (list :no-data state-data)
	 (message "Cannot connect to server for %s, and not enough data for offline operation"
		  (plist-get state-data :address))
	 (list nil state-data))))
    (`(:authenticated ,_)
     ;; That's good enough for now; let's create the directory.
     ;; Mail directories should not be group- or world-readable.
     (with-file-modes #o700
       (make-directory (plist-get state-data :dir) t))
     ;; Let's save the hostname+port+type combo that was successful.
     (with-temp-buffer
       (pcase-let ((`(,hostname ,port ,connection-type)
		    (plist-get state-data :chosen-candidate)))
	 (insert
	  (cl-ecase connection-type
	    (:starttls "imap")
	    (:plaintls "imaps"))
	  "://"
	  (url-hexify-string (plist-get state-data :username))
	  "@"
	  hostname
	  ;; Only include port if it's not the default port
	  (pcase (cons connection-type port)
	    (`(:starttls . 143) "")
	    (`(:plaintls . 993) "")
	    (_ (format ":%d" port)))
	  "\n")
	 (write-region (point-min) (point-max)
		       (expand-file-name "connection-info"
					 (plist-get state-data :dir))
		       nil :silent)))
       (list :connected state-data))))

(define-enter-state bic-account :existing
  (fsm state-data)
  ;; Let's try connecting, but we do have some offline data, and can
  ;; use that if we can't connect.
  (let* ((dir (plist-get state-data :dir))
	 (connection-url
	  (with-temp-buffer
	    (insert-file-contents-literally (expand-file-name "connection-info" dir))
	    (goto-char (point-min))
	    (buffer-substring (point-min) (point-at-eol))))
	 (parsed-url (url-generic-parse-url connection-url))
	 (username (url-unhex-string (url-user parsed-url)))
	 (server (url-host parsed-url))
	 (connection
	  (start-bic-connection
	   username
	   server
	   (pcase (url-type parsed-url)
	     ("imap" :starttls)
	     ("imaps" :plaintls)
	     (bad-type (error "Invalid URL protocol `%s'" bad-type)))
	   (pcase (url-port parsed-url)
	     (0 nil)
	     (port port))
	   (lambda (connection status)
	     (fsm-send fsm (list status connection))))))
    (plist-put state-data :connection connection)
    (plist-put state-data :username username)
    (plist-put state-data :server server)
    (list state-data nil)))

(define-state bic-account :existing
  (fsm state-data event _callback)
  (pcase event
    (`((:disconnected ,keyword ,reason) ,_)
     (unless (plist-get state-data :ever-connected)
       (message "Initial connection to %s@%s failed: %s (%s)"
		(plist-get state-data :username)
		(plist-get state-data :server)
		reason
		keyword))
     (list :disconnected state-data))
    (`(:authenticated ,_)
     (list :connected state-data))))

(define-enter-state bic-account :connected
  (fsm state-data)
  (plist-put state-data :ever-connected t)
  (plist-put state-data :selected nil)
  ;; Find pending flag changes
  (let* ((default-directory (plist-get state-data :dir))
	 (pending-flags-files (file-expand-wildcards "*/pending-flags"))
	 ;; TODO: trim empty files?
	 (mailboxes-with-pending-flags
	  (mapcar
	   (lambda (pending-flags-file)
	     (bic--unsanitize-mailbox-name
	      (directory-file-name
	       (file-name-directory pending-flags-file))))
	   pending-flags-files))
	 (pending-flags-tasks (mapcar (lambda (mailbox)
					(list mailbox :pending-flags))
				      mailboxes-with-pending-flags))
	 (download-messages-tasks
	  ;; TODO: do something clever here.
	  (list (list "INBOX" :download-messages))))
    ;; NB: Overwriting any existing tasks from previous connections.
    (plist-put state-data :tasks (append pending-flags-tasks
					 download-messages-tasks))
    (plist-put state-data :current-task nil))

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
     (pcase list-response
       (`(:ok ,_ ,list-data)
	(bic--handle-list-response state-data list-data)
	;; TODO: open extra connection(s), start downloading interesting
	;; messages from interesting mailboxes

	;; XXX: do we _really_ want an extra connection?..  We could
	;; probably do that later.
	(bic--maybe-next-task fsm state-data)
	(list :connected state-data nil))
       ;; TODO: handle LIST error
       ))
    (`(:select-response ,mailbox-name ,select-response)
     (pcase select-response
       (`(:ok ,_ ,select-data)
	(bic--handle-select-response state-data mailbox-name select-data)

	;; Check if we're doing a task here
	(pcase (plist-get state-data :current-task)
	  ((and `(,(pred (string= mailbox-name)) . ,_)
		current-task)
	   (bic--do-task fsm state-data current-task))
	  (current-task
	   ;; TODO: next task?
	   (when current-task
	     (warn "doing nothing about %S" current-task))
	  ))
	(list :connected state-data nil))
       ;; TODO: handle SELECT error
       ))
    (`(:early-fetch-response ,selected-mailbox ,msg ,uidvalidity)
     (let* ((dir (bic--mailbox-dir state-data selected-mailbox))
	    (overview-file (expand-file-name "overview" dir))
	    (overview-table (bic--read-overview state-data selected-mailbox))
	    (flags-file (expand-file-name "flags" dir))
	    (flags-table (bic--read-flags-table state-data selected-mailbox))
	    (coding-system-for-write 'binary))
       (pcase msg
	 (`(,_seq "FETCH" ,msg-att)
	  ;; TODO: check for mailbox view
	  (let* ((uid-entry (member "UID" msg-att))
		 (uid (cadr uid-entry))
		 (full-uid (when uid-entry (concat uidvalidity "-" uid)))
		 (body-entry (member "BODY" msg-att))
		 (envelope-entry (member "ENVELOPE" msg-att))
		 (flags-entry (member "FLAGS" msg-att)))

	    (let ((existing-flags (gethash full-uid flags-table :not-found))
		  (new-flags (cadr flags-entry)))
	      ;; TODO: do something clever if we don't know the UID
	      (unless (or (null full-uid)
			  (null flags-entry)
			  (equal existing-flags new-flags))
		(puthash full-uid
			 (cadr flags-entry)
			 flags-table)
		;; In the flags file, later entries override earlier
		;; ones, so appending flags is safe.
		;; TODO: rewrite flag file at suitable times
		(with-temp-buffer
		  (insert full-uid " ")
		  (let ((print-escape-newlines t))
		    (prin1 (bic--expand-literals new-flags)
			   (current-buffer)))
		  (insert "\n")
		  (write-region (point-min) (point-max)
				flags-file :append :silent))))

	    (pcase body-entry
	      ((guard (null uid))
	       (warn "Missing UID in FETCH response: %S" msg))
	      (`("BODY" nil (,start-marker . ,end-marker) . ,_)
	       ;; If we do more clever fetching at some point, we'd
	       ;; have a non-nil section-spec.
	       (with-current-buffer (marker-buffer start-marker)
		 (write-region
		  start-marker end-marker
		  (expand-file-name full-uid dir)
		  nil :silent))
	       (set-marker start-marker nil)
	       (set-marker end-marker nil)
	       (let ((envelope-data (bic--expand-literals (cadr envelope-entry))))
		 ;; TODO: avoid duplicates
		 (with-temp-buffer
		   (insert full-uid " ")
		   ;; TODO: better format?
		   ;; XXX: escape CR?
		   (let ((print-escape-newlines t))
		     (prin1 envelope-data (current-buffer)))
		   (insert "\n")
		   (write-region (point-min) (point-max)
				 overview-file :append :silent))
		 (puthash full-uid envelope-data overview-table)))
	      (`("BODY" . ,other)
	       (message "Unexpected BODY in FETCH response: %S" other))
	      (other
	       ;; TODO: only warn if message absent in overview?
	       (message "Missing BODY in FETCH response: %S" other)))

	    (unless (null full-uid)
	      (bic-mailbox--maybe-update-message
	       (plist-get state-data :address)
	       selected-mailbox full-uid))))
	 (other
	  (message "Unexpected response to FETCH request: %S" other)))
       (list :connected state-data)))
    (`(:get-mailbox-tables ,mailbox)
     (funcall callback
	      (list (bic--read-overview state-data mailbox)
		    (bic--read-flags-table state-data mailbox)))
     (list :connected state-data))
    (`(:task-finished ,task)
     ;; Check that this actually is the task we think we're doing
     ;; right now.
     (if (not (eq task (plist-get state-data :current-task)))
	 (warn "Task %S finished while doing task %S"
	       task (plist-get state-data :current-task))
       (plist-put state-data :current-task nil)
       (bic--maybe-next-task fsm state-data))
     (list :connected state-data))
    ((and `(:flags ,mailbox ,full-uid ,flags-to-add ,flags-to-remove)
	  (let `(,uidvalidity ,uid) (split-string full-uid "-")))
     (cond
      ((string= mailbox (plist-get state-data :selected))
       ;; We want to change flags in the mailbox that's currently
       ;; selected - that's easy.
       (if (not (string= uidvalidity
			 (plist-get (cdr
				     (assoc mailbox (plist-get state-data :mailboxes)))
				    :uidvalidity)))
	   ;; TODO: handle this somehow
	   (warn "Cannot change flags in %s; uidvalidity mismatch (%s vs %s)"
		 mailbox uidvalidity (plist-get (cdr
						 (assoc mailbox (plist-get state-data :mailboxes)))
						:uidvalidity))
	 (let* ((current-flags (gethash full-uid (bic--read-flags-table state-data mailbox)))
		(need-to-add (cl-set-difference flags-to-add current-flags :test 'string=))
		(need-to-remove (cl-intersection current-flags flags-to-remove :test 'string=)))
	   ;; TODO: remove duplication
	   ;; TODO: handle untagged responses generally, not per request
	   (when need-to-add
	     (bic-command
	      (plist-get state-data :connection)
	      (concat "UID STORE " uid " +FLAGS ("
		      (mapconcat #'identity need-to-add " ")
		      ")")
	      (lambda (store-response)
		(unless (eq (car store-response) :ok)
		  (message "Couldn't change flags: %s"
			   (plist-get (cl-second store-response) :text))))
	      (list
	       (list 1 "FETCH"
		     (lambda (one-fetch-response)
		       (fsm-send fsm (list :early-fetch-response
					   mailbox
					   one-fetch-response
					   uidvalidity)))))))
	   (when need-to-remove
	     (bic-command
	      (plist-get state-data :connection)
	      (concat "UID STORE " uid " -FLAGS ("
		      (mapconcat #'identity need-to-remove " ")
		      ")")
	      (lambda (store-response)
		(unless (eq (car store-response) :ok)
		  (message "Couldn't change flags: %s"
			   (plist-get (cl-second store-response) :text))))
	      (list
	       (list 1 "FETCH"
		     (lambda (one-fetch-response)
		       (fsm-send fsm (list :early-fetch-response
					   mailbox
					   one-fetch-response
					   uidvalidity))))))))))
      (t
       (bic--write-pending-flags mailbox full-uid flags-to-add flags-to-remove state-data)
       (plist-put state-data
		  :tasks (append (plist-get state-data :tasks)
				 (list (list mailbox :pending-flags))))
       (bic--maybe-next-task fsm state-data)))
     (list :connected state-data))
    (:activate
     ;; Nothing to do.
     (list :connected state-data))
    (:deactivate
     (plist-put state-data :deactivated t)
     ;; Ask server to disconnect.
     (bic-command (plist-get state-data :connection) "LOGOUT" #'ignore)
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

(define-enter-state bic-account :disconnected
  (fsm state-data)
  (unless (plist-get state-data :deactivated)
    (run-with-timer
     bic-reconnect-interval nil
     #'fsm-send fsm :reconnect))
  (list state-data nil))

(define-state bic-account :disconnected
  (fsm state-data event callback)
  (pcase event
    (`(:get-mailbox-tables ,mailbox)
     (funcall callback
	      (list (bic--read-overview state-data mailbox)
		    (bic--read-flags-table state-data mailbox)))
     (list :disconnected state-data))
    (`(:flags ,mailbox ,full-uid ,flags-to-add ,flags-to-remove)
     (bic--write-pending-flags mailbox full-uid flags-to-add flags-to-remove state-data)
     (list :disconnected state-data))
    (:reconnect
     (if (plist-get state-data :deactivated)
	 ;; User asked us not to reconnect.
	 (list :disconnected state-data)
       (list :existing state-data)))
    (:activate
     (plist-put state-data :deactivated nil)
     (fsm-send fsm :reconnect)
     (list :disconnected state-data))
    (:deactivate
     (plist-put state-data :deactivated t)
     (list :disconnected state-data))))

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

(defun bic--unsanitize-mailbox-name (directory-name)
  "Convert a directory name to a UTF-7 mailbox name."
  (let ((i 0) segments)
    (while (string-match "_\\([[:xdigit:]]+\\)_" directory-name i)
      (push (substring directory-name i (match-beginning 0)) segments)
      (push (string (string-to-number (match-string 1 directory-name) 16)) segments)
      (setq i (match-end 0)))
    (push (substring directory-name i) segments)
    (utf7-encode (apply 'concat (nreverse segments)) t)))

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

	  (bic--read-flags-table state-data mailbox-name)
	  (bic--read-overview state-data mailbox-name))
      (warn "Missing UIDVALIDITY!  This is not good.")))

  (plist-put state-data :selected mailbox-name))

(defun bic--maybe-next-task (fsm state-data)
  (unless (plist-get state-data :current-task)
    (let* ((tasks (plist-get state-data :tasks))
	   (task (pop tasks))
	   (mailbox (car task)))
      (when task
	(plist-put state-data :current-task task)
	(plist-put state-data :tasks tasks)
	(if (string= mailbox (plist-get state-data :selected))
	    (bic--do-task fsm state-data task)
	  (bic-command
	   (plist-get state-data :connection)
	   (concat "SELECT " (bic-quote-string mailbox))
	   (lambda (select-response)
	     (fsm-send fsm (list :select-response mailbox select-response)))))))))

(defun bic--do-task (fsm state-data task)
  (pcase task
    (`(,mailbox :pending-flags)
     ;; At this point, we should have selected the mailbox already.
     (cl-assert (string= mailbox (plist-get state-data :selected)))
     (let ((pending-flags-file
	    (expand-file-name "pending-flags" (bic--mailbox-dir state-data mailbox)))
	   (mailbox-uidvalidity
	    (plist-get (cdr (assoc mailbox
				   (plist-get state-data :mailboxes)))
		       :uidvalidity))
	   (flag-combinations (make-hash-table :test 'equal))
	   (discarded 0)
	   file-offset)
       ;; Find sets of messages that have identical sets of flags
       ;; applied, to minimise the number of commands we need to send.
       (with-temp-buffer
	 (insert-file-contents-literally pending-flags-file)
	 (goto-char (point-min))
	 (while (search-forward-regexp
		 (concat "^\\([0-9]+\\)-\\([0-9]+\\)\\([+-]\\)\\(.*\\)$")
		 nil t)
	   (let* ((uidvalidity (match-string 1))
		  (uid (match-string 2))
		  (add-remove (match-string 3))
		  (flags (car (read-from-string (match-string 4)))))
	     (if (string= uidvalidity mailbox-uidvalidity)
		 (push uid (gethash (cons add-remove flags) flag-combinations))
	       (cl-incf discarded))))
	 (setq file-offset (point)))
       (unless (zerop discarded)
	 (warn "Discarding %d pending flag changes for %s"
	       discarded mailbox))
       (let ((remaining-entries-count (hash-table-count flag-combinations))
	     (all-successful t))
	 (if (zerop remaining-entries-count)
	     ;; Nothing to do.
	     (fsm-send fsm (list :task-finished task))
	   (maphash
	    (lambda (add-remove-flags uids)
	      (let ((add-remove (car add-remove-flags))
		    (flags (cdr add-remove-flags))
		    (ranges
		     (gnus-compress-sequence
		      (sort (mapcar 'string-to-number uids) '<)
		      t)))
		;; On a 32-bit Emacs, ranges may contain floating point
		;; numbers - but they're exact enough to represent 32-bit
		;; integers.
		(bic-command
		 (plist-get state-data :connection)
		 (concat "UID STORE " (bic-format-ranges ranges) " "
			 add-remove "FLAGS ("
			 ;; No need to quote flags; they should be atoms.
			 (mapconcat #'identity flags " ")
			 ")")
		 (lambda (store-response)
		   (cl-decf remaining-entries-count)
		   (unless (eq (car store-response) :ok)
		     (warn "Couldn't change flags in %s: %s"
			   mailbox
			   (plist-get (cl-second store-response) :text))
		     ;; Ensure we don't clear the pending flags file
		     (setq all-successful nil))
		   ;; All completed?  If so, clear pending flags file
		   ;; up to the point where we read the last entry -
		   ;; but keep it if there was an error.
		   (when (zerop remaining-entries-count)
		     (when all-successful
		       (with-temp-buffer
			 (insert-file-contents-literally pending-flags-file)
			 (write-region file-offset (point-max) pending-flags-file
				       nil :silent)))
		     (fsm-send fsm (list :task-finished task))))
		 (list
		  (list 1 "FETCH"
			(lambda (one-fetch-response)
			  (fsm-send fsm (list :early-fetch-response
					      mailbox
					      one-fetch-response
					      mailbox-uidvalidity))))))))
	    flag-combinations)))))
    (`(,mailbox :download-messages)
     ;; At this point, we should have selected the mailbox already.
     (cl-assert (string= mailbox (plist-get state-data :selected)))
     (bic-command
      (plist-get state-data :connection)
      ;; XXX: SEARCH or UID SEARCH?
      (concat "UID SEARCH OR OR UNSEEN FLAGGED SINCE "
	      (bic--date-text
	       (time-subtract (current-time)
			      (days-to-time bic-backlog-days))))
      (lambda (search-response)
	(bic--handle-search-response fsm state-data task search-response))))
    (_
     (warn "Unknown task %S" task))))

(defun bic--handle-search-response (fsm state-data task search-response)
  ;; Handle search response by fetching the body of all messages that
  ;; we don't have yet.
  (pcase search-response
    (`(:ok ,_ ,search-data)
     (let* ((search-results (cdr (assoc "SEARCH" search-data)))
	    (mailbox (car task))
	    (uidvalidity
	     (plist-get (cdr (assoc mailbox (plist-get state-data :mailboxes)))
			:uidvalidity))
	    (overview-table (bic--read-overview state-data mailbox))
	    ;; Don't fetch messages we've already downloaded.
	    ;; TODO: detect and react to flag changes
	    ;; TODO: is it possible that we have the envelope, but not the body?
	    (filtered-search-results
	     (cl-remove-if
	      (lambda (uid) (gethash (concat uidvalidity "-" uid) overview-table))
	      search-results)))
       (when filtered-search-results
	 (let* ((count (length filtered-search-results))
		(progress
		 (make-progress-reporter
		  (format "Fetching %d messages from %s..."
			  count mailbox)
		  0 count))
		(n 0))
	   ;; These should be UIDs, since they are a response to a UID
	   ;; SEARCH command.
	   ;; XXX: use plain search and plain fetch instead?
	   (bic-command
	    (plist-get state-data :connection)
	    (concat "UID FETCH "
		    (mapconcat #'identity filtered-search-results ",")
		    ;; TODO: Is "BODY.PEEK[]" the right choice?
		    " (ENVELOPE INTERNALDATE FLAGS BODY.PEEK[])")
	    (lambda (fetch-response)
	      (progress-reporter-done progress)
	      (pcase fetch-response
		(`(:ok ,_ ,fetched-messages)
		 (when fetched-messages
		   (message "Extra response lines for FETCH: %S" fetched-messages)))
		(other
		 (warn "FETCH request failed: %S" other)))
	      (fsm-send fsm (list :task-finished task)))
	    (list
	     (list 1 "FETCH"
		   (lambda (one-fetch-response)
		     (cl-incf n)
		     (progress-reporter-update progress n)
		     (fsm-send fsm (list :early-fetch-response
					 mailbox
					 one-fetch-response
					 uidvalidity))))))))
       (list :connected state-data nil))
     ;; TODO: handle SEARCH error
     )))

(defun bic--read-overview (state-data mailbox-name)
  (let ((overview-table (gethash mailbox-name (plist-get state-data :overview-per-mailbox)))
	(overview-file (expand-file-name "overview" (bic--mailbox-dir state-data mailbox-name))))
    (when (null overview-table)
      (setq overview-table (make-hash-table :test 'equal))
      (puthash mailbox-name overview-table (plist-get state-data :overview-per-mailbox))

      ;; TODO: are there situations where we need to reread the overview file?
      (when (file-exists-p overview-file)
	(with-temp-buffer
	  (insert-file-contents-literally overview-file)
	  (goto-char (point-min))
	  (while (search-forward-regexp
		  (concat "^\\([0-9]+-[0-9]+\\) \\(.*\\)$")
		  nil t)
	    (let ((full-uid (match-string 1))
		  (rest (match-string 2)))
	      (pcase-let
		  ((`(,overview . ,_) (read-from-string rest)))
		(puthash full-uid overview overview-table)))))))
    overview-table))

(defun bic--read-flags-table (state-data mailbox-name)
  (let ((flags-table (gethash mailbox-name (plist-get state-data :flags-per-mailbox))))
    (when (null flags-table)
      (setq flags-table (make-hash-table :test 'equal))
      (puthash mailbox-name flags-table (plist-get state-data :flags-per-mailbox))

      (let* ((dir (bic--mailbox-dir state-data mailbox-name))
	     (flags-file (expand-file-name "flags" dir))
	     (pending-flags-file (expand-file-name "pending-flags" dir)))
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
		  (puthash full-uid flags flags-table))))))
	(when (file-exists-p pending-flags-file)
	  (with-temp-buffer
	    (insert-file-contents-literally pending-flags-file)
	    (goto-char (point-min))
	    (while (search-forward-regexp
		    (concat "^\\([0-9]+-[0-9]+\\)\\([+-]\\)\\(.*\\)$")
		    nil t)
	      (let* ((full-uid (match-string 1))
		     (add-remove (match-string 2))
		     (flags (car (read-from-string (match-string 3))))
		     (existing-flags (gethash full-uid flags-table))
		     (new-flags
		      (pcase add-remove
			("+" (cl-union existing-flags flags :test 'string=))
			("-" (cl-set-difference existing-flags flags :test 'string=)))))
		(puthash full-uid (cl-adjoin :pending new-flags) flags-table)))))))
    flags-table))

(defun bic--write-pending-flags (mailbox full-uid flags-to-add flags-to-remove state-data)
  (pcase-let ((`(,uidvalidity ,_uid) (split-string full-uid "-"))
	      (stored-uid-validity
	       (plist-get (cdr (assoc mailbox (plist-get state-data :mailboxes)))
			  :uidvalidity)))
    (if (and stored-uid-validity
	     (not (string= uidvalidity stored-uid-validity)))
	;; TODO: handle this somehow
	(warn "Cannot change flags in %s; uidvalidity mismatch (%s vs %s)"
	      mailbox uidvalidity stored-uid-validity)
      (let* ((flags-table (bic--read-flags-table state-data mailbox))
	     (current-flags (gethash full-uid flags-table))
	     (need-to-add (cl-set-difference flags-to-add current-flags :test 'string=))
	     (need-to-remove (cl-intersection current-flags flags-to-remove :test 'string=))
	     (pending-flags-file (expand-file-name
				  "pending-flags"
				  (bic--mailbox-dir state-data mailbox))))
	(when (or need-to-add need-to-remove)
	  (with-temp-buffer
	    (when need-to-add
	      (insert full-uid "+")
	      (let ((print-escape-newlines t))
		(prin1 need-to-add (current-buffer)))
	      (insert "\n"))
	    (when need-to-remove
	      (insert full-uid "-")
	      (let ((print-escape-newlines t))
		(prin1 need-to-remove (current-buffer)))
	      (insert "\n"))
	    (write-region (point-min) (point-max)
			  pending-flags-file :append :silent)))
	(puthash full-uid
		 ;; Note that the entry in the hash table does not
		 ;; correspond to what's in the "flags" file.
		 (cl-adjoin :pending
			    (append
			     need-to-add
			     (cl-set-difference current-flags need-to-remove :test 'string=)))
		 flags-table)
	(bic-mailbox--maybe-update-message
	 (plist-get state-data :address)
	 mailbox full-uid)))))

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

(defface bic-mailbox-unread
  '((t (:inherit gnus-summary-normal-unread)))
  "Face used for unread messages."
  :group 'bic)

(defface bic-mailbox-read
  '((t (:inherit gnus-summary-normal-read)))
  "Face used for read messages."
  :group 'bic)

(defface bic-mailbox-flagged
  '((t (:inherit gnus-summary-normal-ticked)))
  "Face used for flagged messages."
  :group 'bic)

(defvar-local bic--current-account nil)

(defvar-local bic--current-mailbox nil)

(defvar-local bic--dir nil)

(defvar-local bic-mailbox--ewoc nil)

(defvar-local bic-mailbox--ewoc-nodes-table nil
  "Hash table mapping uidvalidity+uid to ewoc nodes.")

(defvar-local bic-mailbox--hashtable nil)

(defvar-local bic-mailbox--flags-table nil)

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

(defun bic-mailbox--find-buffer (account mailbox)
  "Return the buffer viewing MAILBOX for ACCOUNT.
If there is no such buffer, return nil."
  (get-buffer (concat mailbox "-" account)))

(defvar bic-mailbox-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") 'bic-mailbox-read-message)
    (define-key map "x" 'bic-mailbox-hide-read)
    (define-key map (kbd "d") 'bic-message-mark-read)
    (define-key map (kbd "M-u") 'bic-message-mark-unread)
    (define-key map "!" 'bic-message-mark-flagged)
    map))

(define-derived-mode bic-mailbox-mode special-mode "BIC mailbox"
  "Major mode for IMAP mailboxes accessed by `bic'."
  (setq header-line-format
	'(" " bic--current-account " " bic--current-mailbox))
  (setq-local revert-buffer-function #'bic-mailbox-reload))

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
    (setq bic-mailbox--ewoc-nodes-table
	  (make-hash-table :test 'equal))
    (bic-mailbox--load-messages)))

(defun bic-mailbox--load-messages ()
  (let* ((uidvalidity-file (expand-file-name "uidvalidity" bic--dir))
	 (uidvalidity (with-temp-buffer
			(insert-file-contents-literally uidvalidity-file)
			(buffer-string)))
	 (messages (directory-files bic--dir nil
				    (concat "^" uidvalidity "-[0-9]+$")))
	 (buffer (current-buffer)))
    (fsm-send
     (bic--find-account bic--current-account)
     (list :get-mailbox-tables bic--current-mailbox)
     (lambda (tables)
       (with-current-buffer buffer
	 (setq bic-mailbox--hashtable (car tables))
	 (setq bic-mailbox--flags-table (cadr tables))
	 (let ((inhibit-read-only t))
	   (dolist (msg messages)
	     (puthash
	      msg (ewoc-enter-last bic-mailbox--ewoc msg)
	      bic-mailbox--ewoc-nodes-table))))))))

(defun bic-mailbox--pp (msg)
  (let ((envelope (gethash msg bic-mailbox--hashtable))
	(flags (gethash msg bic-mailbox--flags-table)))
    (pcase envelope
      (`(,date ,subject (,from . ,_) . ,_)
       ;; TODO: nicer format
       (insert
	(propertize
	 (concat
	  (bic-mailbox--format-flags flags) " "
	  (bic-mailbox--format-date date) "\t[ "
	  (format "%-20.20s"
		  (if (not (string= (car from) "NIL"))
		      (rfc2047-decode-string (car from))
		    (concat (nth 2 from) "@" (nth 3 from))))
	  " ] " (rfc2047-decode-string subject))
	 'face (bic-mailbox--face-from-flags flags))))
      (`nil
       (warn "Message %s not found in hash table" msg)))))

(defun bic-mailbox--face-from-flags (flags)
  (cond
   ((member "\\Flagged" flags)
    'bic-mailbox-flagged)
   ((member "\\Seen" flags)
    'bic-mailbox-read)
   (t
    'bic-mailbox-unread)))

(defun bic-mailbox--format-flags (flags)
  (propertize
   (format "%c%c"
	   (cond
	    ((member "\\Flagged" flags)
	     ?!)
	    ((member "\\Seen" flags)
	     ?R)
	    (t
	     ?\s))
	   (cond
	    ((member "\\Answered" flags)
	     ?A)
	    (t
	     ?\s)))
   'help-echo (concat "Flags: "
		      (if flags
			  (mapconcat 'identity (remq :pending flags) ", ")
			"none"))))

(defun bic-mailbox--format-date (date)
  (let ((parsed-date (ignore-errors (date-to-time date))))
    (if (null date)
	;; cannot parse
	"**********"
      (let ((days (- (time-to-days (current-time)) (time-to-days parsed-date))))
	(cond
	 ((= days 0)
	  ;; same day: show time
	  (format-time-string "     %H:%M" parsed-date))
	 ((< days 365)
	  ;; less than a year ago: show date without year
	  (format "%10s" (format-time-string "%e %b" parsed-date)))
	 (t
	  ;; more than a year ago, or in the future: show YYYY-MM-DD
	  (format-time-string "%F" parsed-date)))))))

(defun bic-mailbox-hide-read ()
  "Hide messages that are marked as read, but not flagged."
  (interactive)
  (unless (derived-mode-p 'bic-mailbox-mode)
    (user-error "Not a mailbox buffer"))
  (ewoc-filter
   bic-mailbox--ewoc
   (lambda (full-uid)
     (let* ((flags (gethash full-uid bic-mailbox--flags-table)))
       (or (member "\\Flagged" flags)
	   (not (member "\\Seen" flags)))))))

(defun bic-mailbox-reload (&optional _ignore-auto _noconfirm)
  "Reload messages for the current mailbox buffer."
  (interactive)
  (unless (derived-mode-p 'bic-mailbox-mode)
    (user-error "Not a mailbox buffer"))
  (let ((inhibit-read-only t))
    ;; First, remove all elements from the ewoc.
    (ewoc-filter bic-mailbox--ewoc #'ignore)
    ;; Then reload.
    (bic-mailbox--load-messages)))

(defun bic-mailbox-read-message ()
  "Open the message under point."
  (interactive)
  (unless (derived-mode-p 'bic-mailbox-mode)
    (user-error "Not a mailbox buffer"))
  (let ((msg (ewoc-data (ewoc-locate bic-mailbox--ewoc (point)))))
    (bic-message-display bic--current-account
			 bic--current-mailbox
			 msg)))

(defun bic-mailbox--maybe-update-message (address mailbox full-uid)
  (pcase (bic-mailbox--find-buffer address mailbox)
    ((and (pred bufferp) mailbox-buffer)
     (run-with-idle-timer
      0.1 nil 'bic-mailbox--update-message mailbox-buffer full-uid))))

(defun bic-mailbox--update-message (buffer full-uid)
  (with-current-buffer buffer
    (pcase (gethash full-uid bic-mailbox--ewoc-nodes-table)
      (`nil
       ;; Not found; add to end of buffer.
       (puthash
	full-uid (ewoc-enter-last bic-mailbox--ewoc full-uid)
	bic-mailbox--ewoc-nodes-table))
      (node
       (ewoc-invalidate bic-mailbox--ewoc node)))))

;;; Message view

(defvar-local bic-message--full-uid nil
  "String containing uidvalidity and uid for message displayed in buffer.")

(defvar bic-message-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    ;; XXX: mark as replied, insert body, etc
    (define-key map "r" 'message-reply)
    (define-key map "d" 'bic-message-mark-read)
    (define-key map (kbd "M-u") 'bic-message-mark-unread)
    (define-key map "!" 'bic-message-mark-flagged)
    ;; (define-key map (kbd "RET") 'bic-mailbox-read-message)
    map))

(define-derived-mode bic-message-mode gnus-article-mode "BIC Message"
  "Major mode for messages viewed from `bic'.")

(defun bic-message-display (account mailbox msg)
  (with-current-buffer (get-buffer-create "*BIC-Message*")
    (let ((inhibit-read-only t)
	  (gnus-summary-buffer (current-buffer))) ;; Don't ask :(
      (bic-message-mode)
      (setq bic--current-account account
	    bic--current-mailbox mailbox
	    bic-message--full-uid msg
	    bic--dir (expand-file-name
		      mailbox
		      (expand-file-name
		       account bic-data-directory)))
      (erase-buffer)
      (remove-overlays)
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

(defun bic-message-mark-read ()
  "Mark the message at point as read.
If the message is marked as flagged, remove the flag."
  (interactive)
  (bic-message-flag '("\\Seen") '("\\Flagged")))

(defun bic-message-mark-unread ()
  "Mark the message at point as unread.
If the message is marked as flagged, remove the flag."
  (interactive)
  (bic-message-flag () '("\\Seen" "\\Flagged")))

(defun bic-message-mark-flagged ()
  "Mark the message at point as flagged.
Also mark it as read."
  (interactive)
  (bic-message-flag '("\\Seen" "\\Flagged") ()))

(defun bic-message-flag (flags-to-add flags-to-remove)
  "Add and remove flags for the message at point."
  (let ((full-uid (bic--find-message-at-point))
	(fsm (bic--find-account bic--current-account)))
    (fsm-send
     fsm
     (list :flags bic--current-mailbox full-uid flags-to-add flags-to-remove))))

(defun bic--find-message-at-point ()
  "Find UID of message at point.
Assert that `bic--current-account' and `bic--current-mailbox' are set.
Return the uidvalidity+uid value of the message under point.
If no message found, signal a `user-error'."
  (unless (and bic--current-account bic--current-mailbox)
    (user-error "Not in mailbox or message buffer"))
  (cond
   ((local-variable-p 'bic-message--full-uid)
    bic-message--full-uid)
   ((local-variable-p 'bic-mailbox--ewoc)
    (pcase (ewoc-locate bic-mailbox--ewoc (point))
      (`nil
       (user-error "No message under point"))
      (node
       (ewoc-data node))))
   (t
    (user-error "Cannot locate message"))))

(provide 'bic)
;;; bic.el ends here
