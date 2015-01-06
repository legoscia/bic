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
(require 'gnus)
(require 'gnus-art)
(require 'gnus-range)
(require 'gnus-srvr)
(require 'hex-util)
(require 'tree-widget)
(require 'avl-tree)

(defgroup bic nil
  "Settings for the Best IMAP Client."
  :group 'mail)

(defvar bic-running-accounts ())

(defvar bic-account-state-table (make-hash-table :test 'equal)
  "Hash table mapping accounts to a state keyword.
Possible values are :connected, :disconnected, and :deactivated.")

(defvar bic-account-state-update-functions ()
  "List of functions called when account state changes.
The functions are called with two argument, the name of the
account, and the new state.  Possible states are the values of
`bic-account-state-table'.

When an account FSM is stopped, these functions are called with
`nil' as the new state.")

(defvar bic-account-mailbox-table (make-hash-table :test 'equal)
  "Hash table of hash tables containing mailbox state.
The keys are account names, and the values are hash tables whose
keys are mailbox names and whose values are plists containing
mailbox state.")

(defvar bic-account-mailbox-update-functions ()
  "List of functions called when mailbox state changes.
The functions are called with three arguments: the name of the
account, the name of the mailbox, and the new state plist.

If the second and third arguments are nil, it means that the
mailbox list has been renewed, and the new list can be retrieved
from `bic-account-mailbox-table'.")

(defvar bic-data-directory (locate-user-emacs-file "bic"))

(defvar bic-backlog-days 30
  "Messages no more than this old will be fetched.")

(defvar bic-reconnect-interval 5
  "Attempt to reconnect after this many seconds.")

(defun bic (&optional new-account)
  "Start BIC.
If there are no configured accounts, or if a prefix argument is
given (setting NEW-ACCOUNT to non-nil), prompt for email address.
Otherwise, start BIC for all known addresses."
  (interactive "P")
  (let ((accounts (directory-files bic-data-directory nil "@")))
    (if (or new-account (null accounts))
	(call-interactively #'bic-add-account)
      (mapc #'bic-add-account
	    (cl-remove-if #'bic--find-account accounts))
      (bic-mailbox-tree))))

(defun bic-add-account (address)
  (interactive "sEmail address: ")
  (if (bic--find-account address)
      (user-error "%s already running" address)
    (push (start-bic-account address) bic-running-accounts)))

(defun bic-deactivate (account)
  "Temporarily deactivate ACCOUNT.
Close any existing connection, and don't attempt to reconnect
until reactivated with `bic-activate'."
  (interactive (list (bic--read-running-account)))
  (fsm-send (bic--find-account account) :deactivate))

(defun bic-activate (account)
  "Reactivate ACCOUNT.
Attempt to reconnect to an account previously disabled with
`bic-deactivate'."
  (interactive (list (bic--read-running-account)))
  (fsm-send (bic--find-account account) :activate))

(defun bic-stop (account)
  "Stop the BIC state machine for ACCOUNT.

If you want to keep using BIC, but stop it from attempting to
reconnect to a certain account, use `bic-deactivate' instead."
  (interactive (list (bic--read-running-account)))
  (fsm-send (bic--find-account account) :stop))

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
	  (bic--update-account-state address :disconnected)
	  (let* ((dir (expand-file-name address bic-data-directory))
		 (state-data (list
			      :address address
			      :dir dir
			      :overview-per-mailbox (make-hash-table :test 'equal)
			      :uid-tree-per-mailbox (make-hash-table :test 'equal)
			      :flags-per-mailbox (make-hash-table :test 'equal))))
	    (if (file-directory-p dir)
		(list :existing state-data)
	      (list :no-data state-data)))))

(define-enter-state bic-account nil
  (fsm state-data)
  (pcase (plist-get state-data :connection)
    ((and connection (pred identity))
     (fsm-send connection :stop)))
  (setq bic-running-accounts (delq fsm bic-running-accounts))
  ;; Need to delete all mailbox buffers, since they reference hash
  ;; tables that are about to go stale.
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'bic-mailbox-mode)
		 (string= bic--current-account (plist-get state-data :address)))
	(kill-buffer buffer))))
  (bic--update-account-state (plist-get state-data :address) nil)
  (list state-data nil))

(define-state bic-account nil
  (_fsm _state-data _event _callback)
  ;; Nothing to do here.
  nil)

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
       (plist-put state-data :connection chosen-connection)
       ;; Close `other-connections':
       (dolist (other-connection other-connections)
	 (fsm-send other-connection :stop))
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
     (pcase-let ((`(,hostname ,port ,connection-type)
		  (plist-get state-data :chosen-candidate)))
       (bic--write-string-to-file
	(concat
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
	(expand-file-name "connection-info" (plist-get state-data :dir))))
     (list :connected state-data))

    (:stop
     (let ((candidate-connections (mapcar #'car (plist-get state-data :candidate-connections))))
       (dolist (connection candidate-connections)
	 (fsm-send connection :stop)))
     (list nil state-data))))

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
    (let ((mailboxes (bic--directory-directories dir "[^.]")))
      (bic--store-initial-mailbox-list
       (plist-get state-data :address)
       (mapcar
	(lambda (mailbox-dir-name)
	  (let ((mailbox (bic--unsanitize-mailbox-name mailbox-dir-name))
		(attributes-file
		 (expand-file-name
		  "attributes"
		  (expand-file-name mailbox-dir-name dir))))
	    (list
	     mailbox
	     :attributes
	     (when (file-exists-p attributes-file)
	       (with-temp-buffer
		 (insert-file-contents-literally attributes-file)
		 (split-string (buffer-string) "\n"))))))
	mailboxes)))
    (plist-put state-data :connection connection)
    (plist-put state-data :username username)
    (plist-put state-data :server server)
    ;; After 60 seconds, consider connection attempt failed.
    (list state-data 60)))

(define-state bic-account :existing
  (fsm state-data event callback)
  (let ((our-connection (plist-get state-data :connection)))
    (pcase event
      (`((:disconnected ,keyword ,reason) ,(pred (eq our-connection)))
       (unless (plist-get state-data :ever-connected)
	 (message "Initial connection to %s@%s failed: %s (%s)"
		  (plist-get state-data :username)
		  (plist-get state-data :server)
		  reason
		  keyword))
       (list :disconnected state-data))
      (:timeout
       (unless (plist-get state-data :ever-connected)
	 (message "Initial connection to %s@%s timed out silently"
		  (plist-get state-data :username)
		  (plist-get state-data :server)))
       (fsm-send our-connection :stop)
       (plist-put state-data :connection nil)
       (list :disconnected state-data))
      (`(:authenticated ,(pred (eq our-connection)))
       (list :connected state-data))
      (`(:flags ,mailbox ,full-uid ,flags-to-add ,flags-to-remove)
       (bic--write-pending-flags mailbox full-uid flags-to-add flags-to-remove state-data)
       (list :existing state-data :keep))
      (`(:get-mailbox-tables ,mailbox)
       (let ((overview-table (bic--read-overview state-data mailbox))
	     (flags-table (bic--read-flags-table state-data mailbox))
	     (uid-tree (gethash mailbox (plist-get state-data :uid-tree-per-mailbox))))
	 (funcall callback (list overview-table flags-table uid-tree)))
       (list :existing state-data :keep))
      (:deactivate
       (plist-put state-data :deactivated t)
       (fsm-send our-connection :stop)
       (plist-put state-data :connection nil)
       (list :disconnected state-data))
      (:stop
       (list nil state-data)))))

(define-enter-state bic-account :connected
  (fsm state-data)
  (bic--update-account-state (plist-get state-data :address) :connected)
  (plist-put state-data :ever-connected t)
  (plist-put state-data :selected nil)
  (plist-put state-data :selecting nil)
  ;; Find pending flag changes
  (let* ((default-directory (plist-get state-data :dir))
	 (pending-flags-files
	  ;; Because of the way we remove entries from pending flags
	  ;; files, we might leave files of size 1 that should be
	  ;; ignored.
	  (cl-remove-if-not
	   (lambda (pending-flags-file)
	     (pcase (nth 7 (file-attributes pending-flags-file))
	       (`nil
		(warn "Cannot open pending flags file `%s'" pending-flags-file)
		nil)
	       (size
		(> size 1))))
	   (file-expand-wildcards "*/pending-flags")))
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
	 (want-subscribed-files (file-expand-wildcards "*/want-subscribed"))
	 (want-subscribed-mailboxes
	  (mapcar
	   (lambda (want-subscribed-file)
	     (bic--unsanitize-mailbox-name
	      (directory-file-name
	       (file-name-directory want-subscribed-file))))
	   want-subscribed-files))
	 (want-subscribed-tasks
	  (mapcar (lambda (mailbox) (list :any-mailbox :subscribe mailbox))
		  want-subscribed-mailboxes)))
    ;; NB: Overwriting any existing tasks from previous connections.
    (plist-put state-data :tasks (append pending-flags-tasks
					 want-subscribed-tasks))
    (plist-put state-data :current-task nil))

  (let ((c (plist-get state-data :connection)))
    ;; If the server supports ID, ask for its identity.  Ignore the
    ;; answer; it's just for the transcript.
    (when (bic-connection--has-capability "ID" c)
      (bic-command c
		   ;; We're anonymous ourselves so far.
		   "ID NIL"
		   #'ignore))

    ;; Get list of mailboxes
    (bic-command c
		 (cond
		  ((bic-connection--has-capability "LIST-EXTENDED" c)
		   (concat
		    "LIST \"\" \"*\" RETURN (SUBSCRIBED"
		    (when (and (bic-connection--has-capability "LIST-STATUS" c)
			       (bic-connection--has-capability "CONDSTORE" c))
		      " STATUS (UIDVALIDITY HIGHESTMODSEQ)")
		    (when (bic-connection--has-capability "SPECIAL-USE" c)
		      " SPECIAL-USE")
		    ")"))
		  (t
		   "LIST \"\" \"*\""))
		 (lambda (response)
		   (fsm-send fsm (list :list-response response)))))
  (list state-data nil))

(define-state bic-account :connected
  (fsm state-data event callback)
  (pcase event
    (`(:list-response ,list-response)
     (pcase list-response
       (`(:ok ,_ ,list-data)
	(bic--handle-list-response fsm state-data list-data)
	;; TODO: open extra connection(s), start downloading interesting
	;; messages from interesting mailboxes

	;; XXX: do we _really_ want an extra connection?..  We could
	;; probably do that later.
	(bic--maybe-next-task fsm state-data)
	(list :connected state-data nil))
       ;; TODO: handle LIST error
       ))
    (`(:lsub-response ,lsub-response)
     (pcase lsub-response
       (`(:ok ,_ ,lsub-data)
	(bic--handle-lsub-response fsm state-data lsub-data)
	(bic--maybe-next-task fsm state-data)
	(list :connected state-data nil))
       ;; TODO: handle LSUB error
       ))
    (`(:select-response ,mailbox-name ,select-response)
     (when (string= mailbox-name (plist-get state-data :selecting))
       (plist-put state-data :selecting nil))
     (pcase select-response
       (`(:ok ,_ ,select-data)
	(bic--handle-select-response state-data mailbox-name select-data)

	;; Check if we're doing a task here
	(pcase (plist-get state-data :current-task)
	  ((and `(,(pred (string= mailbox-name)) . ,_)
		current-task)
	   (bic--do-task fsm state-data current-task))
	  (current-task
	   (when current-task
	     ;; This shouldn't happen
	     (warn "doing nothing about %S" current-task))
	   (bic--maybe-next-task fsm state-data)))
	(list :connected state-data nil))
       (`(,(or :no :bad) ,response ,_response-lines)
	(warn "Cannot select %s for %s: %s"
	      mailbox-name (plist-get state-data :address)
	      (or (plist-get response :text)
		  (plist-get response :code)
		  ""))
	(plist-put state-data :selected nil)
	;; Check if we were trying to select this mailbox for the
	;; current task.
	(pcase (plist-get state-data :current-task)
	  ((and `(,(pred (string= mailbox-name)) . ,_)
		current-task)
	   (plist-put state-data :current-task nil)
	   (bic--maybe-next-task fsm state-data)))
	(list :connected state-data nil))))
    (`(:early-fetch-response ,selected-mailbox ,msg ,uidvalidity)
     (let* ((dir (bic--mailbox-dir state-data selected-mailbox))
	    (overview-file (expand-file-name "overview" dir))
	    (overview-table (bic--read-overview state-data selected-mailbox))
	    (uid-tree (gethash selected-mailbox (plist-get state-data :uid-tree-per-mailbox)))
	    (flags-file (expand-file-name "flags" dir))
	    (flags-table (bic--read-flags-table state-data selected-mailbox))
	    (coding-system-for-write 'binary))
       (pcase msg
	 (`(,_seq "FETCH" ,msg-att)
	  (let* ((uid-entry (member "UID" msg-att))
		 (uid (cadr uid-entry))
		 (full-uid (when uid-entry (concat uidvalidity "-" uid)))
		 (body-entry (member "BODY" msg-att))
		 (envelope-entry (member "ENVELOPE" msg-att))
		 (flags-entry (member "FLAGS" msg-att)))

	    (let ((existing-flags (gethash full-uid flags-table))
		  (new-flags (cadr flags-entry)))
	      ;; TODO: do something clever if we don't know the UID
	      (unless (or (null full-uid)
			  (null flags-entry)
			  (equal existing-flags new-flags)
			  ;; Don't save flags for messages we don't
			  ;; know anything else about.
			  (and (null envelope-entry)
			       (null (gethash full-uid overview-table))))
		(puthash full-uid
			 (cadr flags-entry)
			 flags-table)
		;; In the flags file, later entries override earlier
		;; ones, so appending flags is safe.
		;; TODO: rewrite flag file at suitable times
		(with-temp-buffer
		  (insert full-uid " ")
		  (let ((print-escape-newlines t))
		    (prin1 (bic-expand-literals new-flags)
			   (current-buffer)))
		  (insert "\n")
		  (write-region (point-min) (point-max)
				flags-file :append :silent))))

	    (pcase body-entry
	      ((guard (null uid))
	       (message "Missing UID in FETCH response: %S" msg))
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
	       (let ((envelope-data (bic-expand-literals (cadr envelope-entry))))
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
		 (puthash full-uid envelope-data overview-table)
		 (avl-tree-enter uid-tree (string-to-number uid))))
	      (`("BODY" . ,other)
	       (warn "Unexpected BODY in FETCH response: %S" other))
	      (other
	       ;; TODO: only warn if message absent in overview?
	       ;; (message "Missing BODY in FETCH response: %S" other)
	       ))

	    (unless (null full-uid)
	      (bic-mailbox--maybe-update-message
	       (plist-get state-data :address)
	       selected-mailbox full-uid))))
	 (other
	  (message "Unexpected response to FETCH request: %S" other)))
       (list :connected state-data)))
    (`(:get-mailbox-tables ,mailbox)
     (let ((overview-table (bic--read-overview state-data mailbox))
	   (flags-table (bic--read-flags-table state-data mailbox))
	   (uid-tree (gethash mailbox (plist-get state-data :uid-tree-per-mailbox))))
       (funcall callback (list overview-table flags-table uid-tree)))
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
    (`(:flags ,mailbox ,full-uid ,flags-to-add ,flags-to-remove)
     (bic--write-pending-flags mailbox full-uid flags-to-add flags-to-remove state-data)
     (let ((tasks (plist-get state-data :tasks))
	   (new-task (list mailbox :pending-flags)))
       (unless (member new-task tasks)
	 (plist-put state-data
		    :tasks (append tasks (list new-task)))))
     (bic--maybe-next-task fsm state-data)
     (list :connected state-data))
    (`(:sync-level ,mailbox ,new-sync-level)
     ;; Update sync level and maybe send subscribe command.
     (when (bic--set-sync-level state-data mailbox new-sync-level)
       (bic--queue-task-if-new
	state-data
	(list :any-mailbox :subscribe mailbox)))
     (list :connected state-data))

    (`(:queue-task ,new-task)
     (bic--queue-task-if-new state-data new-task)
     (bic--maybe-next-task fsm state-data)
     (list :connected state-data))
    (`(:idle-timeout ,idle-gensym)
     ;; 29 minutes have passed.  Break our IDLE command, to ensure the
     ;; server doesn't close the connection.
     (pcase (plist-get state-data :current-task)
       (`(:idle ,(pred (eq idle-gensym)) ,_idle-timer)
	(bic--idle-done fsm state-data)))
     (list :connected state-data))
    (`(:idle-done-timeout ,idle-gensym)
     (pcase (plist-get state-data :current-task)
       (`(:idle-done ,(pred (eq idle-gensym)) ,_idle-timer)
	(message "Timeout while leaving IDLE; connection for %s considered lost"
		 (plist-get state-data :address))
	(fsm-send (plist-get state-data :connection) :stop)
	(list :disconnected state-data))
       (_
	;; Timer message received out of order; ignore.
	(list :connected state-data))))
    (`(:idle-response (,idle-type ,idle-extra ,idle-responses) ,idle-gensym)
     ;; An IDLE command has finished, for whatever reason.
     (unless (eq idle-type :ok)
       (warn "IDLE response not OK: %S %S" idle-type idle-extra))
     (when idle-responses
       (warn "Unhandled responses while idle: %S" idle-responses))
     (pcase (plist-get state-data :current-task)
       ;; XXX: "or" correct?
       (`(,(or :idle :idle-done) ,(pred (eq idle-gensym)) ,idle-timer)
	(cancel-timer idle-timer)
	(plist-put state-data :current-task nil)))
     (bic--maybe-next-task fsm state-data)
     (list :connected state-data))
    (`(:ensure-up-to-date ,mailbox)
     (bic--queue-task-if-new
      state-data
      `(,mailbox :sync-mailbox
		 ,@(unless (eq :unlimited-sync (bic--mailbox-sync-level state-data mailbox))
		     '(:limit 100))))
     (bic--maybe-next-task fsm state-data)
     (list :connected state-data))
    (:activate
     ;; Nothing to do.
     (list :connected state-data))
    (:deactivate
     (plist-put state-data :deactivated t)
     ;; Ask server to disconnect - but exit IDLE first, if needed.
     (plist-put state-data :tasks (list (list :any-mailbox :logout)))
     (bic--maybe-next-task fsm state-data)
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
       (list :connected state-data))))
    (:stop
     (list nil state-data))))

(define-enter-state bic-account :disconnected
  (fsm state-data)
  (bic--update-account-state
   (plist-get state-data :address)
   (if (plist-get state-data :deactivated)
       :deactivated
     :disconnected))
  (unless (plist-get state-data :deactivated)
    (run-with-timer
     bic-reconnect-interval nil
     #'fsm-send fsm :reconnect))
  (list state-data nil))

(define-state bic-account :disconnected
  (fsm state-data event callback)
  (pcase event
    (`(:get-mailbox-tables ,mailbox)
     (let ((overview-table (bic--read-overview state-data mailbox))
	   (flags-table (bic--read-flags-table state-data mailbox))
	   (uid-tree (gethash mailbox (plist-get state-data :uid-tree-per-mailbox))))
       (funcall callback (list overview-table flags-table uid-tree)))
     (list :disconnected state-data))
    (`(:flags ,mailbox ,full-uid ,flags-to-add ,flags-to-remove)
     (bic--write-pending-flags mailbox full-uid flags-to-add flags-to-remove state-data)
     (list :disconnected state-data))
    (`(:sync-level ,mailbox ,new-sync-level)
     ;; Update sync level; we may have to send subscribe commands when
     ;; we are connected.
     (bic--set-sync-level state-data mailbox new-sync-level)
     (list :disconnected state-data))
    (:reconnect
     (if (plist-get state-data :deactivated)
	 ;; User asked us not to reconnect.
	 (list :disconnected state-data)
       (list :existing state-data)))
    (:activate
     (plist-put state-data :deactivated nil)
     (bic--update-account-state (plist-get state-data :address) :disconnected)
     (fsm-send fsm :reconnect)
     (list :disconnected state-data))
    (:deactivate
     (plist-put state-data :deactivated t)
     (bic--update-account-state (plist-get state-data :address) :deactivated)
     (list :disconnected state-data))
    (:stop
     (list nil state-data))))

(defun bic--update-account-state (account new-state)
  (let ((old-state (gethash account bic-account-state-table)))
    (unless (eq old-state new-state)
      (if (null new-state)
	  (remhash account bic-account-state-table)
	(puthash account new-state bic-account-state-table))
      ;; Run in timer, to isolate errors.
      (run-with-timer
       0.1 nil
       #'run-hook-with-args 'bic-account-state-update-functions
       account new-state))))

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

(defun bic--handle-list-response (fsm state-data list-data)
  (let* ((old-mailboxes (plist-get state-data :mailboxes))
	 ;; If the server supports LIST-EXTENDED, then we will have
	 ;; asked for mailbox subscription information.
	 (subscription-return
	  (bic-connection--has-capability "LIST-EXTENDED" (plist-get state-data :connection)))
	 mailboxes)
    ;; XXX: do we really always overwrite mailbox information?
    ;; Need to reverse responses, so we get LIST before STATUS.
    (dolist (x (nreverse list-data))
      (pcase x
	(`("LIST" ,attributes ,_separator ,mailbox-name)
	 ;; If we weren't asking for subscription information,
	 ;; keep previous subscription status.
	 (let* ((old-attributes
		 (unless subscription-return
		   (plist-get (cdr (assoc mailbox-name old-mailboxes)) :attributes)))
		(subscription-attribute
		 (when (cl-member "\\Subscribed" old-attributes :test #'cl-equalp)
		   (list "\\Subscribed"))))
	   (push
	    (list mailbox-name
		  :attributes (append subscription-attribute attributes))
	    mailboxes)))
	(`("STATUS" ,mailbox-name ,status-att-list)
	 (let ((mailbox-entry (assoc mailbox-name mailboxes)))
	   (if (null mailbox-entry)
	       (warn "STATUS response for unknown mailbox %S" mailbox-name)
	     (pcase (member "UIDVALIDITY" status-att-list)
	       (`("UIDVALIDITY" ,uidvalidity . ,_)
		(setf (cdr mailbox-entry)
		      (plist-put (cdr mailbox-entry) :uidvalidity uidvalidity))))
	     (pcase (member "HIGHESTMODSEQ" status-att-list)
	       (`("HIGHESTMODSEQ" ,highest-modseq . ,_)
		(setf (cdr mailbox-entry)
		      (plist-put (cdr mailbox-entry) :server-modseq highest-modseq)))))))
	(_
	 (warn "Unexpected LIST response: %S" x))))
    (mapc
     (lambda (mailbox)
       (let* ((dir (bic--mailbox-dir state-data (car mailbox)))
	      (attributes (plist-get (cdr mailbox) :attributes))
	      (sync-level-file (expand-file-name "sync-level" dir))
	      (sync-level-file-contents
	       (and (file-exists-p sync-level-file)
		    (with-temp-buffer
		      (insert-file-contents-literally sync-level-file)
		      (buffer-string))))
	      (sync-level-keyword
	       (and sync-level-file-contents
		    (cdr (assoc sync-level-file-contents
				'(("unlimited-sync" . :unlimited-sync)
				  ("partial-sync" . :partial-sync)
				  ("no-sync" . :no-sync)))))))
	 (make-directory dir t)
	 (bic--write-string-to-file
	  (mapconcat #'identity attributes "\n")
	  (expand-file-name "attributes" dir))
	 (setf (cdr mailbox)
	       (plist-put (cdr mailbox) :explicit-sync-level sync-level-keyword))))
     mailboxes)
    ;; Modify state-data in place:
    (plist-put state-data :mailboxes mailboxes)
    (bic--store-initial-mailbox-list (plist-get state-data :address) mailboxes)
    (if subscription-return
	;; If we have subscription info, we can sync mailboxes.
	(bic--queue-task-if-new state-data (list :any-mailbox :sync-mailboxes))
      ;; If we didn't get the subscription info already, ask for it with LSUB.
      (bic-command
       (plist-get state-data :connection)
       "LSUB \"\" \"*\""
       (lambda (response)
    	 (fsm-send fsm (list :lsub-response response)))))))

(defun bic--handle-lsub-response (_fsm state-data lsub-data)
  (let ((mailboxes (plist-get state-data :mailboxes))
	(subscribed-mailboxes
	 (delq nil
	       (mapcar
		(lambda (lsub)
		  (pcase lsub
		    (`("LSUB" ,_attributes ,_separator ,mailbox-name)
		     mailbox-name)))
		lsub-data))))
    (mapc
     (lambda (mailbox-entry)
       (let* ((mailbox (car mailbox-entry))
	      (old-attributes (plist-get (cdr mailbox-entry) :attributes))
	      new-attributes
	      (just-subscribed (and (member mailbox subscribed-mailboxes)
				    (not (member "\\Subscribed" old-attributes))))
	      (just-unsubscribed (and (member "\\Subscribed" old-attributes)
				      (not (member mailbox subscribed-mailboxes)))))
	 (cond
	  (just-subscribed
	   (setq new-attributes (cons "\\Subscribed" old-attributes)))
	  (just-unsubscribed
	   (setq new-attributes (remove "\\Subscribed" old-attributes))))
	 (when (or just-subscribed just-unsubscribed)
	   ;; TODO: update mailbox entries for mailbox tree
	   (setf (cdr mailbox-entry) (plist-put (cdr mailbox-entry) :attributes new-attributes))
	   (bic--write-string-to-file
	    (mapconcat #'identity new-attributes "\n")
	    (expand-file-name "attributes" (bic--mailbox-dir state-data mailbox)))
	   (bic--update-mailbox-status (plist-get state-data :address)
				       (car mailbox-entry) (cdr mailbox-entry)))))
     mailboxes)
    ;; Now we can finally sync the mailboxes.
    (bic--queue-task-if-new state-data (list :any-mailbox :sync-mailboxes))))

(defun bic--sync-mailboxes (fsm state-data task)
  ;; TODO: do we need to issue a new LIST-STATUS request here?
  ;; XXX: would that overwrite everything we know?
  (let ((download-messages-tasks
	 (cl-mapcan
	  (lambda (mailbox-data)
	    (let* ((dir (bic--mailbox-dir state-data (car mailbox-data)))
		   (modseq-file (expand-file-name "modseq" dir))
		   (needs-sync
		    (or (not (file-exists-p modseq-file))
			(null (plist-get (cdr mailbox-data) :uidvalidity))
			(null (plist-get (cdr mailbox-data) :server-modseq))
			(let ((our-modseq-pair
			       (with-temp-buffer
				 (insert-file-contents-literally modseq-file)
				 (split-string (buffer-string) "-"))))
			  (or (not (string=
				    (car our-modseq-pair)
				    (plist-get (cdr mailbox-data) :uidvalidity)))
			      (bic--numeric-string-lessp
			       (cadr our-modseq-pair)
			       (plist-get (cdr mailbox-data) :server-modseq)))))))
	      (when needs-sync
		(cl-case (bic--mailbox-sync-level state-data (car mailbox-data))
		  (:unlimited-sync
		   (list (list (car mailbox-data) :sync-mailbox)))
		  (:partial-sync
		   (list (list (car mailbox-data) :sync-mailbox :limit 100)))))))
	  (plist-get state-data :mailboxes))))
    (plist-put state-data :tasks
	       (append (plist-get state-data :tasks)
		       download-messages-tasks))
    (fsm-send fsm (list :task-finished task))))

(defun bic--set-sync-level (state-data mailbox new-sync-level)
  "Update sync level for MAILBOX in STATE-DATA.
If we should send a subscribe command, write a \"want-subscribed\"
file and return t."
  (let* ((mailbox-data (assoc mailbox (plist-get state-data :mailboxes)))
	 (attributes (plist-get (cdr mailbox-data) :attributes))
	 (want-subscribed (memq new-sync-level '(unlimited-sync partial-sync))))
    (setf (cdr mailbox-data)
	  (plist-put (cdr mailbox-data)
		     :explicit-sync-level
		     (cdr (assq new-sync-level
				'((unlimited-sync . :unlimited-sync)
				  (partial-sync . :partial-sync)
				  (no-sync . :no-sync))))))
    (bic--update-mailbox-status (plist-get state-data :address)
				mailbox (cdr mailbox-data))
    (let* ((dir (bic--mailbox-dir state-data mailbox))
	   (sync-level-file (expand-file-name "sync-level" dir)))
      (with-temp-buffer
	(insert (symbol-name new-sync-level))
	(write-region (point-min) (point-max)
		      sync-level-file nil :silent)))
    ;; Ensure that the mailbox is subscribed if we want to sync it.
    (when (and want-subscribed
	       (not (cl-member "\\Subscribed" attributes :test #'cl-equalp)))
      (write-region
       (point-min) (point-min)
       (expand-file-name
	(bic--mailbox-dir state-data mailbox)
	"want-subscribed")
       nil :silent)
      t)))

(defun bic--mailbox-sync-level (state-data mailbox)
  (let ((mailbox-data (assoc mailbox (plist-get state-data :mailboxes))))
    (bic--infer-sync-level mailbox-data)))

(defun bic--infer-sync-level (mailbox-data)
  (let ((attributes (plist-get (cdr mailbox-data) :attributes))
	(explicit-sync-level (plist-get (cdr mailbox-data) :explicit-sync-level)))
    (cond
     ((or (cl-member "\\Noselect" attributes :test #'cl-equalp)
	  (cl-member "\\NonExistent" attributes :test #'cl-equalp))
      nil)
     (explicit-sync-level
      (if (eq explicit-sync-level :no-sync)
	  nil
	explicit-sync-level))
     ((cl-member "\\Subscribed" attributes :test #'cl-equalp)
      :partial-sync))))

(defun bic--store-initial-mailbox-list (address mailboxes)
  (let ((table (gethash address bic-account-mailbox-table)))
    (if table
	(clrhash table)
      (setq table (make-hash-table :test 'equal))
      (puthash address table bic-account-mailbox-table))
    (mapc
     (lambda (mailbox)
       (puthash (car mailbox) (cdr mailbox) table))
     mailboxes)
    ;; Run in timer, to isolate errors.
    (run-with-timer
     0.1 nil
     #'run-hook-with-args 'bic-account-mailbox-update-functions
     address nil nil)))

(defun bic--update-mailbox-status (address mailbox new-plist)
  (let ((table (gethash address bic-account-mailbox-table)))
    (puthash mailbox new-plist table)
    ;; Run in timer, to isolate errors.
    (run-with-timer
     0 nil
     #'run-hook-with-args 'bic-account-mailbox-update-functions
     address mailbox new-plist)))

(defun bic--handle-select-response (state-data mailbox-name select-data)
  (cl-flet ((find-entry (type)
			(cl-find-if
			 (lambda (x)
			   (and (eq (car-safe x) :ok)
				(string= type (plist-get (cdr x) :code))))
			 select-data))
	    (find-entry-number-first (type)
			(cl-find-if
			 (lambda (x)
			   (and (stringp (cl-second x))
				(string= type (cl-second x))))
			 select-data)))
    (let ((dir (bic--mailbox-dir state-data mailbox-name))
	  (uidvalidity-entry (find-entry "UIDVALIDITY"))
	  (highestmodseq-entry (find-entry "HIGHESTMODSEQ"))
	  (nomodseq-entry (find-entry "NOMODSEQ"))
	  (exists-entry (find-entry-number-first "EXISTS")))
      (if uidvalidity-entry
	  (let ((uidvalidity-file (expand-file-name "uidvalidity" dir))
		(modseq-file (expand-file-name "modseq" dir))
		(uidvalidity (plist-get (cdr uidvalidity-entry) :data))
		(mailbox-entry (assoc mailbox-name (plist-get state-data :mailboxes))))
	    (when (null mailbox-entry)
	      (warn "Selecting unknown mailbox `%s'" mailbox-name)
	      (setq mailbox-entry (list mailbox-name))
	      (plist-put state-data :mailboxes
			 (cons mailbox-entry (plist-get state-data :mailboxes))))
	    (setf (cdr mailbox-entry)
		  (plist-put (cdr mailbox-entry) :uidvalidity uidvalidity))
	    (if (file-exists-p uidvalidity-file)
		(with-temp-buffer
		  (insert-file-contents-literally uidvalidity-file)
		  (unless (string= (buffer-string) uidvalidity)
		    ;; TODO: do something sensible here
		    (warn "UIDVALIDITY mismatch: %s vs %s"
			  (buffer-string) uidvalidity)))
	      (message "Fresh UIDVALIDITY value: %S" uidvalidity-entry)
	      (bic--write-string-to-file uidvalidity uidvalidity-file))

	    (if (null exists-entry)
		(warn "No EXISTS response found for mailbox %s of %s"
		      mailbox-name (plist-get state-data :address))
	      (plist-put (cdr mailbox-entry) :exists (string-to-number (cl-first exists-entry))))

	    (cond
	     (highestmodseq-entry
	      (let ((old-modseq
		     (when (file-exists-p modseq-file)
		       (with-temp-buffer
			 (insert-file-contents-literally modseq-file)
			 (split-string (buffer-string) "-"))))
		    (new-modseq (plist-get (cdr highestmodseq-entry) :data)))
		(plist-put (cdr mailbox-entry) :our-modseq
			   (when (and old-modseq
				      (string= (car old-modseq) uidvalidity))
			     (cadr old-modseq)))
		(plist-put (cdr mailbox-entry) :server-modseq new-modseq)))
	     ((or nomodseq-entry
		  (not (bic-connection--has-capability
			"CONDSTORE" (plist-get state-data :connection))))
	      (when (file-exists-p modseq-file)
		(delete-file modseq-file))
	      (plist-put (cdr mailbox-entry) :our-modseq nil)
	      (plist-put (cdr mailbox-entry) :server-modseq nil))
	     (t
	      (warn "Neither HIGHESTMODSEQ nor NOMODSEQ reported for mailbox %s of %s"
		    mailbox-name (plist-get state-data :address))))

	    (bic--read-flags-table state-data mailbox-name)
	    (bic--read-overview state-data mailbox-name))
	(warn "Missing UIDVALIDITY!  This is not good.")))

    (plist-put state-data :selected mailbox-name)))

(defun bic--queue-task-if-new (state-data task)
  (let ((existing-tasks (plist-get state-data :tasks)))
    (unless (member task existing-tasks)
      (plist-put state-data :tasks
		 (append existing-tasks (list task))))))

(defun bic--maybe-next-task (fsm state-data)
  (let ((tasks (plist-get state-data :tasks))
	(selected-mailbox (plist-get state-data :selected))
	(c (plist-get state-data :connection))
	(selecting (plist-get state-data :selecting)))
    (pcase (plist-get state-data :current-task)
      ((guard selecting)
       ;; If we're in the process of selecting a new mailbox, don't
       ;; issue new commands.
       nil)
      ((and `(:idle ,_idle-gensym ,idle-timer)
	    (guard tasks))
       ;; If we're in IDLE, and there are pending tasks, interrupt
       ;; the IDLE command.
       (cancel-timer idle-timer)
       (bic--idle-done fsm state-data)
       ;; The OK response to IDLE will trigger a new call to
       ;; `bic--maybe-next-task'.
       )
      ((and `nil (guard tasks))
       ;; If we're outside any running command, and there are pending
       ;; tasks, switch to the correct mailbox if necessary, or just
       ;; do the task.
       (let* ((task (pop tasks))
	      (mailbox (car task)))
	 (plist-put state-data :current-task task)
	 (plist-put state-data :tasks tasks)
	 (if (or (eq mailbox :any-mailbox)
		 (string= mailbox selected-mailbox))
	     (bic--do-task fsm state-data task)
	   (bic--select fsm state-data mailbox))))
      ((and `nil
	    (guard (null tasks))
	    (guard (bic-connection--has-capability "IDLE" c)))
       ;; If we're outside any running command, and there are no
       ;; pending tasks, and the server supports IDLE, issue an IDLE
       ;; command - but ensure that we have selected INBOX first.
       ;; TODO: do something clever to sync other mailboxes too.
       (if (string= "INBOX" (plist-get state-data :selected))
	   (bic--idle fsm state-data)
	 (bic--select fsm state-data "INBOX"))))))

(defun bic--select (fsm state-data mailbox)
  "Issue a SELECT command for MAILBOX, and send response to FSM."
  (let ((c (plist-get state-data :connection)))
    (plist-put state-data :selecting mailbox)
    (bic-command
     c
     (concat "SELECT " (bic-quote-string mailbox)
	     (when (bic-connection--has-capability "CONDSTORE" c)
	       " (CONDSTORE)"))
     (lambda (select-response)
       (fsm-send fsm (list :select-response mailbox select-response))))))

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
	   (flag-change-uids (make-hash-table :test 'equal))
	   (discarded 0)
	   file-offset)
       ;; Find sets of messages that have identical sets of flags
       ;; applied, to minimise the number of commands we need to send.
       (when (file-exists-p pending-flags-file)
	 (with-temp-buffer
	   (insert-file-contents-literally pending-flags-file)
	   (goto-char (point-min))
	   (while (search-forward-regexp
		   (concat "^\\([0-9]+\\)-\\([0-9]+\\)\\([+-]\\)\\(.*\\)$")
		   nil t)
	     (let* ((uidvalidity (match-string 1))
		    (uid (match-string 2))
		    (add-remove (match-string 3))
		    (flag (match-string 4))
		    (opposite (car (remove add-remove '("+" "-")))))
	       (if (string= uidvalidity mailbox-uidvalidity)
		   ;; The pending flags file may contain an
		   ;; instruction to set a flag for a message and then
		   ;; clear it, or vice versa.  Ensure that we only
		   ;; apply the last such instruction.
		   (let* ((opposite-key (cons opposite flag))
			  (opposite-uids (gethash opposite-key flag-change-uids)))
		     (when (member uid opposite-uids)
		       (setq opposite-uids (delete uid opposite-uids))
		       (if (null opposite-uids)
			   (remhash opposite-key flag-change-uids)
			 (puthash opposite-key
				  opposite-uids
				  flag-change-uids)))
		     (push uid (gethash (cons add-remove flag) flag-change-uids)))
		 (cl-incf discarded))))
	   (setq file-offset (point))))
       (unless (zerop discarded)
	 (warn "Discarding %d pending flag changes for %s"
	       discarded mailbox))
       (let ((c (plist-get state-data :connection))
	     (remaining-entries-count (hash-table-count flag-change-uids))
	     (all-successful t))
	 (if (zerop remaining-entries-count)
	     ;; Nothing to do.
	     (fsm-send fsm (list :task-finished task))
	   (maphash
	    (lambda (add-remove-flag uids)
	      (let ((add-remove (car add-remove-flag))
		    (flag (cdr add-remove-flag))
		    (ranges
		     (gnus-compress-sequence
		      (sort (mapcar 'string-to-number uids) '<)
		      t)))
		;; On a 32-bit Emacs, ranges may contain floating point
		;; numbers - but they're exact enough to represent 32-bit
		;; integers.
		(bic-command
		 c
		 (concat "UID STORE " (bic-format-ranges ranges) " "
			 add-remove "FLAGS ("
			 ;; No need to quote flags; they should be atoms.
			 flag ")")
		 (lambda (store-response)
		   ;; XXX: check additional responses
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
				       nil :silent))
		       ;; If we added the \Deleted flag, also expunge:
		       (pcase (gethash '("+" . "\\Deleted") flag-change-uids)
			 (`nil
			  ;; Nothing to expunge, task finished.
			  (fsm-send fsm (list :task-finished task)))
			 (deleted-uids
			  (let ((expunge-command
				 (if (bic-connection--has-capability "UIDPLUS" c)
				     ;; We can use UID EXPUNGE to
				     ;; expunge only the affected
				     ;; messages.
				     (concat
				      "UID EXPUNGE "
				      (bic-format-ranges
				       (gnus-compress-sequence
					(sort (mapcar #'string-to-number deleted-uids)
					      #'<)
					t)))
				   ;; The server doesn't support
				   ;; UIDPLUS.  Let's just expunge
				   ;; everything that's marked
				   ;; \Deleted, like Gnus does.
				   "EXPUNGE")))
			    (bic-command
			     c expunge-command
			     (lambda (expunge-response)
			       (let ((response-lines (cl-third expunge-response)))
				 (when response-lines
				   ;; TODO: handle
				   (warn "Unhandled responses to expunge request: %s"
					 response-lines)))
			       (unless (eq (car expunge-response) :ok)
				 (warn "Couldn't expunge messages in %s: %s"
				       mailbox
				       (plist-get (cl-second expunge-response) :text)))
			       ;; Done expunging, task finished.
			       (fsm-send fsm (list :task-finished task))))))))))
		 (list
		  (list 1 "FETCH"
			(lambda (one-fetch-response)
			  (fsm-send fsm (list :early-fetch-response
					      mailbox
					      one-fetch-response
					      mailbox-uidvalidity))))))))
	    flag-change-uids)))))
    (`(,mailbox :sync-mailbox . ,_options)
     ;; At this point, we should have selected the mailbox already.
     (cl-assert (string= mailbox (plist-get state-data :selected)))
     ;; TODO: do something clever with the limit, so we don't need to
     ;; dig through too much data.
     (let* ((connection (plist-get state-data :connection))
	    unseen-flagged-search-data recent-search-data
	    (mailbox-plist
	     (cdr (assoc mailbox (plist-get state-data :mailboxes))))
	    (uidvalidity (plist-get mailbox-plist :uidvalidity))
	    (our-modseq (plist-get mailbox-plist :our-modseq))
	    (server-modseq (plist-get mailbox-plist :server-modseq))
	    (highest-modseq our-modseq)
	    (prefix (concat uidvalidity "-"))
	    (overview-table (bic--read-overview state-data mailbox))
	    uids)

       ;; We can skip requesting flags for already downloaded messages
       ;; if there aren't any downloaded messages, or if the server
       ;; supports CONDSTORE, and the MODSEQ values match.
       (when (and (not (zerop (hash-table-count overview-table)))
		  (or (null server-modseq)
		      (null our-modseq)
		      (bic--numeric-string-lessp our-modseq server-modseq)))
	 ;; Find list of UIDs present in overview table.
	 (maphash
	  (lambda (full-uid _overview-data)
	    (when (string-prefix-p prefix full-uid)
	      (let ((uid (substring full-uid (1+ (cl-position ?- full-uid)))))
		(push uid uids))))
	  overview-table)
	 ;; XXX: if the mailbox has many messages, and the server
	 ;; doesn't support CONDSTORE, this will take really long
	 ;; time, as we unconditionally get the flag status of every
	 ;; message we know about.
	 (bic-uids-command
	  connection
	  "UID FETCH "
	  (gnus-compress-sequence (sort (mapcar 'string-to-number uids) '<) t)
	  (cond
	   ((bic-connection--has-capability "CONDSTORE" connection)
	    (if our-modseq
		(concat " FLAGS (CHANGEDSINCE " our-modseq ")")
	      " (FLAGS MODSEQ)"))
	   (t
	    " FLAGS"))
	  (lambda (worst-response all-responses)
	    (cl-ecase worst-response
	      (:ok
	       (let (extra-response-lines)
		 (dolist (response all-responses)
		   (setq extra-response-lines
			 (append extra-response-lines (cl-third response)))
		   (let ((resp (cl-second response)))
		     ;; Check for HIGHESTMODSEQ response code.
		     ;; If none, use highest MODSEQ seen in FETCH responses.
		     (when (string= (plist-get resp :code) "HIGHESTMODSEQ")
		       (setq highest-modseq (plist-get resp :data)))))
		 (when extra-response-lines
		   (message "Extra response lines for FETCH: %S" extra-response-lines))))
	      ((:no :bad)
	       (warn "FETCH request failed: %S"
		     (cl-remove :ok all-responses :key #'car)))))
	  (list
	   (list 1 "FETCH"
		 (lambda (one-fetch-response)
		   ;; If the FETCH response contains a MODSEQ item,
		   ;; remember the highest one.
		   (pcase one-fetch-response
		     (`(,_ "FETCH" ,msg-att)
		      (pcase (member "MODSEQ" msg-att)
			(`("MODSEQ" (,msg-modseq) . ,_)
			 (when (or (null highest-modseq)
				   (bic--numeric-string-lessp
				    highest-modseq msg-modseq))
			   (setq highest-modseq msg-modseq))))))
		   (fsm-send fsm (list :early-fetch-response
				       mailbox
				       one-fetch-response
				       uidvalidity)))))))
       ;; Now search for messages that we need to download.
       (bic-command
	connection
	(concat "UID SEARCH "
		(when (and t (bic-connection--has-capability "ESEARCH" connection))
		  "RETURN () ")
		"OR UNSEEN FLAGGED")
	(lambda (search-response)
	  (pcase search-response
	    (`(:ok ,_ ,search-data)
	     (setq unseen-flagged-search-data search-data))
	    (other
	     (warn "SEARCH for unseen/flagged messages failed: %S" other)))))
       (bic-command
	(plist-get state-data :connection)
	(concat "UID SEARCH "
		(when (and t (bic-connection--has-capability "ESEARCH" connection))
		  "RETURN () ")
		"SEEN UNFLAGGED SINCE "
		(bic--date-text
		 (time-subtract (current-time)
				(days-to-time bic-backlog-days))))
	(lambda (search-response)
	  (pcase search-response
	    (`(:ok ,_ ,search-data)
	     (setq recent-search-data search-data))
	    (other
	     (warn "SEARCH for recent messages failed: %S" other)))
	  ;; At this point, both requests should have finished.
	  (bic--handle-search-response
	   fsm state-data task
	   unseen-flagged-search-data recent-search-data
	   highest-modseq server-modseq)))))

    (`(,mailbox :expunge-messages . ,sequence-numbers)
     (cl-assert (string= mailbox (plist-get state-data :selected)))
     ;; Since we don't (yet?) maintain a mapping of sequence numbers
     ;; to UIDs, we need to issue a SEARCH command to find out which
     ;; messages are affected.
     (setq sequence-numbers (mapcar #'string-to-number sequence-numbers))
     (let* ((connection (plist-get state-data :connection))
	    (mailbox-plist (cdr (assoc mailbox (plist-get state-data :mailboxes))))
	    (mailbox-uidvalidity (plist-get mailbox-plist :uidvalidity))
	    (exists (plist-get mailbox-plist :exists))
	    (overview-table (bic--read-overview state-data mailbox))
	    (uid-tree (gethash mailbox (plist-get state-data :uid-tree-per-mailbox)))
	    (dir (bic--mailbox-dir state-data mailbox))
	    (overview-file (expand-file-name "overview" dir))
	    ;; XXX: split list if numbers have gaps?
	    (min (apply #'min sequence-numbers))
	    (max (apply #'max sequence-numbers))
	    (highest-expunged (>= max exists)))
       (bic-command
	connection
	(format "UID SEARCH %s:%s"
		;; Ask for all message sequence numbers starting from
		;; one below the lowest that has been expunged.  If
		;; the lowest expunged message is 1, there is nothing
		;; lower so go with 1.
		(bic-number-to-string (max (1- min) 1))
		;; If the highest numbered message in the mailbox
		;; seems to have been expunged, search for "*" just to
		;; be sure.
		(if highest-expunged
		    "*"
		  (bic-number-to-string max)))
	(lambda (search-response)
	  (pcase search-response
	    (`(:ok ,_ ,search-data)
	     (let* ((search-results (cdr (assoc "SEARCH" search-data)))
		    (sorted (sort (mapcar #'string-to-number search-results) #'<))
		    ;; If the lowest expunged message was 1, any UID
		    ;; from the earliest we know about could have been
		    ;; expunged.  TODO: this could be rather
		    ;; inefficient.
		    (min-uid (if (= min 1) (avl-tree-first uid-tree) (cl-first sorted)))
		    ;; If the highest expunged message is greater than
		    ;; the number of messages in the mailbox, then the
		    ;; results contain the highest UID present in the
		    ;; mailbox (since we asked for it above).  Thus,
		    ;; check all messages in mailbox.
		    (max-uid (if highest-expunged
				 (avl-tree-last uid-tree)
			       (car (last sorted)))))
	       ;; TODO: It's a bit weird to use an index to loop over
	       ;; a hashtable.  Ideally, we'd want to loop over part
	       ;; of the AVL tree, but it seems there is no such
	       ;; function.
	       (cl-loop
		for uid from min-uid to max-uid
		do (if (and sorted (= uid (car sorted)))
		       ;; This UID is present in the search results,
		       ;; thus this message still exists.
		       (pop sorted)
		     (let ((full-uid (concat mailbox-uidvalidity "-" (bic-number-to-string uid))))
		       (when (gethash full-uid overview-table)
			 ;; This UID is not present in the search
			 ;; results, but present in our hashtable.
			 ;; Thus it is a message that we know about,
			 ;; that has been expunged.
			 (with-temp-buffer
			   (insert full-uid " :expunged\n")
			   (write-region (point-min) (point-max)
					 overview-file :append :silent))
			 (remhash full-uid overview-table)
			 (avl-tree-delete uid-tree uid)
			 (bic-mailbox--maybe-remove-message
			  (plist-get state-data :address)
			  mailbox full-uid)
			 ;; TODO: remove file on disk once this works properly
			 ))))))
	    (search-error
	     (warn "Error in response to SEARCH: %S" search-error)))
	  (fsm-send fsm (list :task-finished task))))))
    (`(:any-mailbox :sync-mailboxes)
     (bic--sync-mailboxes fsm state-data task))
    (`(:any-mailbox :subscribe ,mailbox)
     (bic-command
      (plist-get state-data :connection)
      (concat "SUBSCRIBE " (bic-quote-string mailbox))
      (lambda (subscribe-response)
	(pcase subscribe-response
	  (`(:ok ,_ ,extra-data)
	   (when extra-data
	     (warn "Extra data in response to SUBSCRIBE: %S" extra-data))
	   (let* ((dir (bic--mailbox-dir state-data mailbox))
		  (want-subscribed-file (expand-file-name "want-subscribed" dir)))
	     (when (file-exists-p want-subscribed-file)
	       (delete-file want-subscribed-file))))
	  (subscribe-error
	   (warn "Error in response to SUBSCRIBE: %S" subscribe-error)))
	(fsm-send fsm (list :task-finished task)))))
    (`(,_ :logout)
     ;; No need for a callback - this is the last task.
     (bic-command (plist-get state-data :connection) "LOGOUT" #'ignore))
    (_
     (warn "Unknown task %S" task))))

(defun bic--idle (fsm state-data)
  (let* ((idle-gensym (cl-gensym "IDLE-"))
	 (timer (run-with-timer (* 29 60) nil
				(lambda ()
				  (fsm-send fsm (list :idle-timeout idle-gensym)))))
	 (mailbox (plist-get state-data :selected))
	 (mailbox-plist (cdr (assoc mailbox (plist-get state-data :mailboxes))))
	 (expunge-task (list mailbox :expunge-messages)))
    (plist-put state-data :current-task (list :idle idle-gensym timer))
    (bic-command
     (plist-get state-data :connection)
     "IDLE"
     (lambda (idle-response)
       (fsm-send fsm (list :idle-response idle-response idle-gensym)))
     ;; TODO: more specific response handlers here
     (list
      (list
       0 :ok
       (lambda (ok-response)
	 (when (plist-get (cdr ok-response) :code)
	   (warn "Unknown response while IDLE: %S %S %S"
		 (plist-get (cdr ok-response) :code)
		 (plist-get (cdr ok-response) :data)
		 (plist-get (cdr ok-response) :text)))))
      (list
       1 "EXISTS"
       (lambda (exists-response)
	 (plist-put mailbox-plist :exists (string-to-number (cl-first exists-response)))
	 ;; TODO: just fetch new messages, as we know their
	 ;; sequence numbers.
	 (fsm-send
	  fsm
	  `(:queue-task
	    (,mailbox :sync-mailbox
		      ,@(unless (eq :unlimited-sync
				    (bic--mailbox-sync-level state-data mailbox))
			  '(:limit 100)))))))
      ;; We don't really care about the \Recent flag.  Assuming
      ;; that the server always sends EXISTS along with RECENT,
      ;; we can ignore this.
      (list 1 "RECENT" #'ignore)
      (list 0 "FLAGS" #'ignore)
      ;; A somewhat complicated dance to ensure that after receiving
      ;; EXPUNGE responses, we'll exit IDLE, and run the expunge task
      ;; once and only once, with all received message sequence
      ;; numbers.  Thus we build the task using `nconc', and send it
      ;; off with `:queue-task', which skips duplicate tasks.
      (list 1 "EXPUNGE"
	    (lambda (expunge-response)
	      (nconc expunge-task (list (cl-first expunge-response)))
	      (fsm-send
	       fsm
	       `(:queue-task ,expunge-task))))))))

(defun bic--idle-done (fsm state-data)
  (let ((idle-gensym (cl-second (plist-get state-data :current-task))))
    (plist-put state-data :current-task
	       (list :idle-done idle-gensym
		     (run-with-timer
		      10 nil
		      (lambda ()
			(fsm-send fsm (list :idle-done-timeout idle-gensym)))))))
  (bic--send (plist-get state-data :connection) "DONE\r\n"))

(defun bic--numeric-string-lessp (s1 s2)
  (cond ((< (length s1) (length s2)) t)
	((> (length s1) (length s2)) nil)
	((string= s1 s2) nil)
	(t (string-lessp s1 s2))))

(defun bic--handle-search-response
    (fsm state-data task unseen-flagged-search-data recent-search-data
	 highest-modseq server-modseq)
  ;; Handle search response by fetching the body of all messages that
  ;; we don't have yet.
  (let* ((unseen-flagged-ranges (bic--get-search-range unseen-flagged-search-data))
	 (unseen-flagged-length (gnus-range-length unseen-flagged-ranges))
	 (recent-ranges (bic--get-search-range recent-search-data))
	 (recent-length (gnus-range-length recent-ranges))
	 (mailbox (car task))
	 (uidvalidity
	  (plist-get (cdr (assoc mailbox (plist-get state-data :mailboxes)))
		     :uidvalidity))
	 (overview-table (bic--read-overview state-data mailbox))
	 (limit (plist-get (cddr task) :limit))
	 fetch-these not-these-unseen not-these-recent
	 (mailbox-table (gethash (plist-get state-data :address) bic-account-mailbox-table))
	 (mailbox-entry (gethash mailbox mailbox-table)))
    (cond
     ((and (numberp limit) (> unseen-flagged-length limit))
      (let* ((sorted (gnus-uncompress-range unseen-flagged-ranges))
	     (cut-point (last sorted (1+ limit))))
	(setq fetch-these (gnus-compress-sequence (cdr cut-point)))
	(setf (cdr cut-point) nil)
	(setq not-these-unseen sorted)
	(setq not-these-recent recent-ranges)))
     ((and (numberp limit) (> (+ unseen-flagged-length recent-length) limit))
      ;; We can fetch all unread/flagged messages, but not all recent
      ;; ones.
      (let* ((sorted-recent
	      (gnus-uncompress-range recent-ranges))
	     (cut-point (last sorted-recent
			      (1+ (- limit unseen-flagged-length)))))
	(setq fetch-these
	      (gnus-range-add
	       (cdr cut-point)
	       unseen-flagged-ranges))
	(setf (cdr cut-point) nil)
	(setq not-these-unseen nil)
	(setq not-these-recent sorted-recent)))
     (t
      (setq fetch-these (gnus-range-add unseen-flagged-ranges recent-ranges))))

    (cl-flet
	((uid-overview (uid)
		       (gethash (concat uidvalidity "-" (bic-number-to-string uid))
				overview-table))
	 (write-modseq
	  ()
	  ;; The server told us the new MODSEQ value when we selected
	  ;; the mailbox.  We retrieved all flag changes since our
	  ;; previously recorded MODSEQ value, and all new messages
	  ;; that we are interested in.  At this point, we should be
	  ;; entitled to bump our recorded MODSEQ value.
	  (when server-modseq
	    (let* ((dir (bic--mailbox-dir state-data mailbox))
		   (modseq-file (expand-file-name "modseq" dir)))
	      (bic--write-string-to-file
	       (concat uidvalidity "-" server-modseq)
	       modseq-file)))))
      ;; Now we know which messages we're not going to fetch because
      ;; they exceed the limit.  If they haven't been downloaded
      ;; already, warn about it.
      ;; (message "Skipping unseen/flagged: %S recent: %S"
      ;; 	       (cl-remove-if #'uid-overview not-these-unseen)
      ;; 	       (cl-remove-if #'uid-overview not-these-recent))
      (cond
       ((cl-member-if-not #'uid-overview (gnus-uncompress-range not-these-unseen))
	(unless (plist-get mailbox-entry :not-all-unread)
	  (warn "%s (%s) has too many new/flagged messages!  Limiting to the latest %d"
		mailbox (plist-get state-data :address) limit)
	  ;; TODO: set a permanent flag
	  (bic--update-mailbox-status
	   (plist-get state-data :address)
	   mailbox
	   (plist-put mailbox-entry :not-all-unread t))))
       ((cl-member-if-not #'uid-overview (gnus-uncompress-range not-these-recent))
	(unless (plist-get mailbox-entry :not-all-recent)
	  ;; TODO: improve wording
	  (warn "%s (%s) has too many recent messages!  Limiting to the latest %d"
		mailbox (plist-get state-data :address) limit)
	  ;; TODO: set a permanent flag
	  (bic--update-mailbox-status
	   (plist-get state-data :address)
	   mailbox
	   (plist-put mailbox-entry :not-all-recent t)))))

      ;; Don't fetch messages we've already downloaded.
      ;; TODO: is it possible that we have the envelope, but not the body?
      (pcase (cl-delete-if #'uid-overview (gnus-uncompress-range fetch-these))
	(`nil
	 (write-modseq)
	 (message "Nothing to fetch from %s for %s"
		  mailbox (plist-get state-data :address))
	 (fsm-send fsm (list :task-finished task)))
	(filtered-search-results
	 (let* ((c (plist-get state-data :connection))
		(count (length filtered-search-results))
		(progress
		 (make-progress-reporter
		  (format "Fetching %d messages from %s for %s..."
			  count mailbox (plist-get state-data :address))
		  0 count))
		(n 0))
	   ;; These should be UIDs, since they are a response to a UID
	   ;; SEARCH command.
	   (bic-command
	    c
	    (concat "UID FETCH "
		    (bic-format-ranges
		     (gnus-compress-sequence
		      (sort filtered-search-results '<)
		      t))
		    ;; TODO: Is "BODY.PEEK[]" the right choice?
		    " ("
		    (when (bic-connection--has-capability "CONDSTORE" c)
		      "MODSEQ ")
		    "ENVELOPE INTERNALDATE FLAGS BODY.PEEK[])")
	    (lambda (fetch-response)
	      (progress-reporter-done progress)
	      (pcase fetch-response
		(`(:ok ,_ ,fetched-messages)
		 (when fetched-messages
		   (message "Extra response lines for FETCH: %S" fetched-messages))
		 ;; TODO: can we use highest-modseq for anything?
		 (write-modseq))
		(other
		 (warn "FETCH request failed: %S" other)))
	      (fsm-send fsm (list :task-finished task)))
	    (list
	     (list 1 "FETCH"
		   (lambda (one-fetch-response)
		     (cl-incf n)
		     (progress-reporter-update progress n)
		     ;; If the FETCH response contains a MODSEQ item,
		     ;; remember the highest one.
		     (pcase one-fetch-response
		       (`(,_ "FETCH" ,msg-att)
			(pcase (member "MODSEQ" msg-att)
			  (`("MODSEQ" (,msg-modseq) . ,_)
			   (when (or (null highest-modseq)
				     (bic--numeric-string-lessp
				      highest-modseq msg-modseq))
			     (setq highest-modseq msg-modseq))))))
		     (fsm-send fsm (list :early-fetch-response
					 mailbox
					 one-fetch-response
					 uidvalidity)))))))))))
  (list :connected state-data nil))

(defun bic--get-search-range (search-data)
  "Return the results in SEARCH-DATA as a list of ranges."
  (let ((esearch (assoc "ESEARCH" search-data)))
    (if esearch
	(bic--get-esearch-range esearch)
      (let ((search (assoc "SEARCH" search-data)))
	(gnus-compress-sequence
	 (sort (mapcar #'string-to-number (cdr search))
	       #'<)
	 t)))))

(defun bic--get-esearch-range (esearch)
  (pcase (member "ALL" esearch)
    (`("ALL" ,ranges-string . ,_)
     (bic-parse-sequence-set ranges-string))
    (_
     ;; RFC 4731:
     ;;
     ;; If the SEARCH results in no matches, the server MUST
     ;; NOT include the ALL result option in the ESEARCH response;
     ;; however, it still MUST send the ESEARCH response.
     nil)))

(defun bic--read-overview (state-data mailbox-name)
  (let* ((dir (bic--mailbox-dir state-data mailbox-name))
	 (overview-table (gethash mailbox-name (plist-get state-data :overview-per-mailbox)))
	 (uid-tree (gethash mailbox-name (plist-get state-data :uid-tree-per-mailbox)))
	 (overview-file (expand-file-name "overview" dir)))
    (when (null overview-table)
      (setq overview-table (make-hash-table :test 'equal))
      (puthash mailbox-name overview-table (plist-get state-data :overview-per-mailbox))
      (setq uid-tree (avl-tree-create #'<))
      (puthash mailbox-name uid-tree (plist-get state-data :uid-tree-per-mailbox))

      ;; TODO: are there situations where we need to reread the overview file?
      (when (file-exists-p overview-file)
	(let* ((uidvalidity-file (expand-file-name "uidvalidity" dir))
	       (uidvalidity (with-temp-buffer
			      (insert-file-contents-literally uidvalidity-file)
			      (buffer-string)))
	       (regexp (concat "^\\(" uidvalidity "-\\([0-9]+\\)\\) \\(.*\\)$")))
	  (with-temp-buffer
	    (insert-file-contents-literally overview-file)
	    (goto-char (point-min))
	    (while (search-forward-regexp regexp nil t)
	      (let ((full-uid (match-string 1))
		    (bare-uid (string-to-number (match-string 2)))
		    (rest (match-string 3)))
		(pcase-let
		    ((`(,overview . ,_) (read-from-string rest)))
		  (if (eq :expunged overview)
		      (progn
			(remhash full-uid overview-table)
			(avl-tree-delete uid-tree bare-uid))
		    (puthash full-uid overview overview-table)
		    (avl-tree-enter uid-tree bare-uid)))))))))
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
	    (dolist (flag need-to-add)
	      (insert full-uid "+" flag "\n"))
	    (dolist (flag need-to-remove)
	      (insert full-uid "-" flag "\n"))
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

(defun bic--write-string-to-file (string file)
  "Write STRING to FILE, overwriting any previous contents."
  (with-temp-buffer
    (insert string)
    (write-region (point-min) (point-max) file nil :silent)))

;;; Mailbox tree

(defface bic-mailbox-tree-account-connected
  '((t (:inherit gnus-server-opened)))
  "Face used for connected accounts in mailbox tree."
  :group 'bic)

(defface bic-mailbox-tree-account-disconnected
  '((t (:inherit gnus-server-denied)))
  "Face used for disconnected accounts in mailbox tree."
  :group 'bic)

(defface bic-mailbox-tree-account-deactivated
  '((t (:inherit gnus-server-closed)))
  "Face used for deactivated accounts in mailbox tree."
  :group 'bic)

(defface bic-mailbox-tree-mailbox-unlimited-sync
  '((t (:inherit gnus-group-mail-1)))
  "Face used for fully synced mailboxes in mailbox tree."
  :group 'bic)

(defface bic-mailbox-tree-mailbox-partial-sync
  '((t (:inherit gnus-group-mail-low)))
  "Face used for partially synced mailboxes in mailbox tree."
  :group 'bic)

(defface bic-mailbox-tree-mailbox-unsubscribed
  '((t (:inherit gnus-group-news-low-empty)))
  "Face used for unsubscribed mailboxes in mailbox tree."
  :group 'bic)

(defvar bic-mailbox-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") 'bic-mailbox-tree-press-button-on-current-line)
    map))

(define-derived-mode bic-mailbox-tree-mode special-mode "BIC mailbox tree"
  "Major mode for tree of IMAP mailboxes accessed by `bic'."
  (add-hook 'bic-account-state-update-functions
	    'bic-mailbox-tree--update-account-state)
  (add-hook 'bic-account-mailbox-update-functions
	    'bic-mailbox-tree--update-mailbox-state)
  (widget-minor-mode)
  (setq-local widget-global-map bic-mailbox-tree-mode-map))

(defun bic-mailbox-tree-press-button-on-current-line (&optional event)
  "Find button on current line and press it.
By default, widget mode is too stingy about where the point has
to be for the button press to count.  Let's try to do what the
user expects."
  (interactive "@d")
  (let ((button (bic--button-on-current-line)))
    (if (null button)
	(user-error "No button on this line")
      (widget-apply-action button event))))

(defun bic--button-on-current-line ()
  (save-excursion
    (forward-line 0)
    (let ((button-pos
	   (if (get-char-property (point) 'button)
	       (point)
	     (next-single-char-property-change
	      (point) 'button nil (line-end-position)))))
      (when button-pos (get-char-property button-pos 'button)))))

(defun bic-mailbox-tree ()
  "Show mailbox tree buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*Mailboxes*")
    (unless (derived-mode-p 'bic-mailbox-tree-mode)
      (bic-mailbox-tree-mode)
      (bic-mailbox-tree--init))
    (switch-to-buffer (current-buffer))))

(defvar-local bic-mailbox-tree--widget nil)

(defun bic-mailbox-tree--init ()
  (unless bic-mailbox-tree--widget
    (setq bic-mailbox-tree--widget
	  (widget-create
	   'tree-widget
	   :tag "Accounts"
	   :open t
	   :expander #'bic-mailbox-tree--accounts
	   :expander-p (lambda (&rest _) t)))))

(defun bic-mailbox-tree--accounts (_parent)
  (mapcar
   (lambda (fsm)
     (let ((address (plist-get (fsm-get-state-data fsm) :address)))
       (bic-mailbox-tree--account address)))
   bic-running-accounts))

(defun bic-mailbox-tree--account (address)
  (widget-convert
   'tree-widget
   :tag (bic-mailbox-tree--account-tag address (gethash address bic-account-state-table))
   :address address
   :expander #'bic-mailbox-tree--mailboxes
   :expander-p (lambda (&rest _) t)))

(defun bic-mailbox-tree--account-tag (address state)
  (propertize
   (format "%s (%s)" address (substring (symbol-name state) 1))
   'face
   (cl-case state
     (:connected 'bic-mailbox-tree-account-connected)
     (:disconnected 'bic-mailbox-tree-account-disconnected)
     (:deactivated 'bic-mailbox-tree-account-deactivated))))

(defun bic-mailbox-tree--update-account-state (account new-state)
  (let ((buffer (get-buffer "*Mailboxes*")))
    (when buffer
      (with-current-buffer buffer
	(let* ((children (widget-get bic-mailbox-tree--widget :children))
	       (account-widget
		(cl-find-if
		 (lambda (child)
		   (and (tree-widget-p child)
			(equal (widget-get child :address) account)))
		 children)))
	  (cond
	   ((and account-widget new-state)
	    ;; State change for exisiting account
	    (let ((node (car (widget-get account-widget :children))))
	      (if (null node)
		  (warn "no node: %S" account-widget)
		(widget-put node :tag (bic-mailbox-tree--account-tag account new-state))
		;; Redraw.
		(widget-value-set node (widget-value node)))))
	   ((or
	     ;; Account removed
	     (and account-widget (null new-state))
	     ;; New account
	     (and new-state (null account-widget)))
	    ;; Just redraw the tree.  TODO: it would be nice to
	    ;; preserve the "open" state of tree nodes.
	    (widget-value-set bic-mailbox-tree--widget (widget-value bic-mailbox-tree--widget)))))))))

(defun bic-mailbox-tree--mailboxes (parent)
  (let* ((account-name (widget-get parent :address))
	 (mailbox-table (gethash account-name bic-account-mailbox-table))
	 mailboxes)
    (when mailbox-table
      (maphash
       (lambda (mailbox data)
	 (push (cons mailbox data) mailboxes))
       mailbox-table))
    (setq mailboxes (cl-sort mailboxes #'string-lessp :key #'car))
    (mapcar
     (lambda (mailbox-data)
       (let ((mailbox-name (car mailbox-data))
	     (attributes (plist-get (cdr mailbox-data) :attributes)))
	 ;; It's unclear whether these attributes are case sensitive
	 ;; or not, so let's use cl-equalp.
	 (if (or (cl-member "\\Noselect" attributes :test #'cl-equalp)
		 (cl-member "\\NonExistent" attributes :test #'cl-equalp))
	     (widget-convert 'item mailbox-name)
	   (widget-convert
	    'link
	    :notify (lambda (&rest _ignore)
		      (bic-mailbox-open account-name (car mailbox-data)))
	    :tag "42"
	    :format (concat
		     "%[%v%] (%t)"
		     (cond
		      ((plist-get (cdr mailbox-data) :not-all-unread)
		       (propertize " [not all unread fetched]"
				   'face 'error))
		      ((plist-get (cdr mailbox-data) :not-all-recent)
		       (propertize " [not all recent fetched]"
				   'face 'warning)))
		     "\n")
	    :button-face (cl-ecase (bic--infer-sync-level mailbox-data)
			   (:unlimited-sync
			    'bic-mailbox-tree-mailbox-unlimited-sync)
			   (:partial-sync
			    'bic-mailbox-tree-mailbox-partial-sync)
			   ((nil)
			    'bic-mailbox-tree-mailbox-unsubscribed))
	    mailbox-name))))
     mailboxes)))

(defun bic-mailbox-tree--update-mailbox-state (account _mailbox _state)
  (let ((buffer (get-buffer "*Mailboxes*")))
    (when buffer
      (with-current-buffer buffer
	;; TODO: check mailbox non-nil
	(let ((account-widget
	       (cl-find-if
		(lambda (child)
		  (and (tree-widget-p child)
		       (equal (widget-get child :address) account)))
		(widget-get bic-mailbox-tree--widget :children))))
	  (when account-widget
	    (widget-value-set account-widget (widget-value account-widget))))))))

;;; Set sync level

(defun bic-mailbox-set-sync-level (account mailbox sync-level)
  (interactive
   (let* ((account (bic--read-existing-account "IMAP account: " t))
	  (mailbox (bic--read-mailbox "Mailbox: " account t))
	  (sync-levels '("unlimited-sync"
			 "partial-sync"
			 "no-sync"))
	  (sync-level (completing-read
		       "New sync level: "
		       sync-levels nil t)))
     (if (member sync-level sync-levels)
	 (list account mailbox (intern sync-level))
       (user-error "No sync level specified"))))
  (fsm-send (bic--find-account account) (list :sync-level mailbox sync-level)))

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

(defface bic-mailbox-spam
  '((t (:inherit spam)))
  "Face used for messages marked as spam."
  :group 'bic)

(defface bic-mailbox-deleted
  '((t (:inherit gnus-summary-cancelled :strike-through t)))
  "Face used for messages marked for deletion."
  :group 'bic)

(defvar-local bic--current-account nil)

(defvar-local bic--current-mailbox nil)

(defvar-local bic--dir nil)

(defvar-local bic-mailbox--ewoc nil)

(defvar-local bic-mailbox--ewoc-nodes-table nil
  "Hash table mapping uidvalidity+uid to ewoc nodes.")

(defvar-local bic-mailbox--hashtable nil)

(defvar-local bic-mailbox--flags-table nil)

(defvar-local bic-mailbox--uid-tree nil)

(defun bic-mailbox-open (account mailbox)
  (interactive
   (let* ((account (bic--read-existing-account "IMAP account: " t))
	  (mailbox (bic--read-mailbox "Mailbox: " account t)))
     (list account mailbox)))
  (let ((buffer-name (concat mailbox "-" account)))
    (with-current-buffer (get-buffer-create buffer-name)
      (if (derived-mode-p 'bic-mailbox-mode)
	  ;; If we already have a mailbox buffer for this mailbox,
	  ;; ensure that it's up to date.
	  (bic-mailbox-update)
	(bic-mailbox-mode)
	(bic-mailbox--init account mailbox)))
    (switch-to-buffer buffer-name)))

(defun bic--read-existing-account (prompt require-match)
  "Read the name of an email account with completion.
If REQUIRE-MATCH is non-nil, only accept accounts that we know
about."
  (let ((accounts (directory-files bic-data-directory nil "@")))
    (completing-read prompt accounts nil require-match)))

(defun bic--directory-directories (dir regexp)
  "Like `directory-files', but only returns directories."
  (cl-remove-if-not
   (lambda (file)
     (file-directory-p (expand-file-name file dir)))
   (directory-files dir nil regexp)))

(defun bic--read-mailbox (prompt account require-match)
  "Read the name of a mailbox for ACCOUNT."
  (let ((mailboxes (bic--directory-directories (expand-file-name account bic-data-directory) "[^.]")))
    (completing-read prompt (mapcar #'bic--unsanitize-mailbox-name mailboxes)
		     nil require-match)))

(defun bic-mailbox--find-buffer (account mailbox)
  "Return the buffer viewing MAILBOX for ACCOUNT.
If there is no such buffer, return nil."
  (get-buffer (concat mailbox "-" account)))

(defvar bic-mailbox-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") 'bic-mailbox-read-message)
    (define-key map "x" 'bic-mailbox-hide-read)
    (define-key map (kbd "M-g") 'bic-mailbox-update)
    (define-key map (kbd "d") 'bic-message-mark-read)
    (define-key map (kbd "M-u") 'bic-message-mark-unread)
    (define-key map "!" 'bic-message-mark-flagged)
    (define-key map (kbd "B DEL") 'bic-message-mark-deleted)
    (define-key map "$" 'bic-message-mark-spam)
    (define-key map "\M-$" 'bic-message-mark-not-spam)
    (define-key map "c" 'bic-mailbox-catchup)
    map))

(define-derived-mode bic-mailbox-mode special-mode "BIC mailbox"
  "Major mode for IMAP mailboxes accessed by `bic'."
  (setq header-line-format
	'(" " bic--current-account " " bic--current-mailbox
	  (:eval
	   (let* ((mailbox-table
		   (gethash bic--current-account bic-account-mailbox-table))
		  (mailbox-entry
		   (and (hash-table-p mailbox-table)
			(gethash bic--current-mailbox mailbox-table))))
	     (cond
	      ((plist-get mailbox-entry :not-all-unread)
	       (propertize " [not all unread fetched]"
			   'face 'error))
	      ((plist-get mailbox-entry :not-all-recent)
	       (propertize " [not all recent fetched]"
			   'face 'warning)))))))
  (setq-local revert-buffer-function #'bic-mailbox-reload)
  (setq-local truncate-lines t))

(defun bic-mailbox--init (account mailbox)
  (setq bic--current-account account
	bic--current-mailbox mailbox
	;; TODO: use bic--mailbox-dir
	bic--dir (expand-file-name
		  (bic--sanitize-mailbox-name mailbox)
		  (expand-file-name
		   account bic-data-directory)))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq bic-mailbox--ewoc
	  (ewoc-create #'bic-mailbox--pp))
    (setq bic-mailbox--ewoc-nodes-table
	  (make-hash-table :test 'equal :weakness 'value))
    (bic-mailbox--load-messages)))

(defun bic-mailbox--load-messages ()
  (let* ((uidvalidity-file (expand-file-name "uidvalidity" bic--dir))
	 (uidvalidity
	  (when (file-exists-p uidvalidity-file)
	    (with-temp-buffer
	      (insert-file-contents-literally uidvalidity-file)
	      (buffer-string))))
	 (buffer (current-buffer)))
    (fsm-send
     (bic--find-account bic--current-account)
     (list :get-mailbox-tables bic--current-mailbox)
     (lambda (tables)
       (with-current-buffer buffer
	 (setq bic-mailbox--hashtable (cl-first tables))
	 (setq bic-mailbox--flags-table (cl-second tables))
	 (setq bic-mailbox--uid-tree (cl-third tables))
	 (let ((inhibit-read-only t))
	   (avl-tree-mapc
	    (lambda (bare-uid)
	      (let ((full-uid (concat uidvalidity "-" (bic-number-to-string bare-uid))))
		(puthash
		 full-uid (ewoc-enter-last bic-mailbox--ewoc full-uid)
		 bic-mailbox--ewoc-nodes-table)))
	    bic-mailbox--uid-tree)))))
    (fsm-send
     (bic--find-account bic--current-account)
     (list :ensure-up-to-date bic--current-mailbox))))

(defun bic-mailbox--pp (msg)
  (let ((envelope (gethash msg bic-mailbox--hashtable))
	(flags (gethash msg bic-mailbox--flags-table)))
    (pcase envelope
      (`(,date ,subject (,from . ,_) . ,_)
       ;; TODO: nicer format
       (cl-flet ((col (n) (propertize " " 'display `(space :align-to ,n))))
	 (insert
	  (propertize
	   (concat
	    (bic-mailbox--format-flags flags)
	    (col 3) (bic-mailbox--format-date date)
	    (col 16) "[ "
	    (truncate-string-to-width
	     (if (member (car from) '("" "NIL"))
		 (concat (nth 2 from) "@" (nth 3 from))
	       (rfc2047-decode-string (car from)))
	     20)
	    (col 39) "] "
	    (rfc2047-decode-string subject))
	   'face (bic-mailbox--face-from-flags flags)))))
      (`nil
       (warn "Message %s not found in hash table" msg)))))

(defun bic-mailbox--face-from-flags (flags)
  (cond
   ((member "\\Deleted" flags)
    'bic-mailbox-deleted)
   ((member "$Junk" flags)
    'bic-mailbox-spam)
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
	    ((member "$Junk" flags)
	     ?$)
	    ((member "\\Flagged" flags)
	     ?!)
	    ((member "\\Seen" flags)
	     ?R)
	    ((member "\\Recent" flags)
	     ?.)
	    (t
	     ?\s))
	   (cond
	    ((member "\\Answered" flags)
	     ?A)
	    ((member "$Forwarded" flags)
	     ?F)
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
	 ((< days 180)
	  ;; less than half a year ago: show date without year
	  (format "%10s" (format-time-string "%e %b" parsed-date)))
	 (t
	  ;; more than half a year ago, or in the future: show YYYY-MM-DD
	  (format-time-string "%F" parsed-date)))))))

(defun bic-mailbox-hide-read ()
  "Hide messages that are marked as read, but not flagged.
Also hide messages marked for deletion."
  (interactive)
  (unless (derived-mode-p 'bic-mailbox-mode)
    (user-error "Not a mailbox buffer"))
  (ewoc-filter
   bic-mailbox--ewoc
   (lambda (full-uid)
     (let* ((flags (gethash full-uid bic-mailbox--flags-table)))
       (and (not (member "\\Deleted" flags))
	    (or (member "\\Flagged" flags)
		(not (member "\\Seen" flags))))))))

(defun bic-mailbox-catchup ()
  "Mark all visible unread messages as read."
  (interactive)
  (unless (derived-mode-p 'bic-mailbox-mode)
    (user-error "Not a mailbox buffer"))
  (let ((messages nil)
	(count 0))
    (ewoc-map
     (lambda (full-uid)
       (unless (member "\\Seen" (gethash full-uid bic-mailbox--flags-table))
	 (push full-uid messages)
	 (incf count))
       nil)
     bic-mailbox--ewoc)
    (if (null messages)
	(message "All visible messages are already marked as read; nothing to catchup")
      (if (y-or-n-p (format "Mark %d messages in %s as read? " count bic--current-mailbox))
	  (let ((fsm (bic--find-account bic--current-account)))
	    ;; TODO: it would be nice to send all UIDs to the FSM in a
	    ;; single message.
	    (dolist (full-uid messages)
	      (fsm-send
	       fsm
	       (list :flags bic--current-mailbox full-uid '("\\Seen") ()))))
	(message "Catchup cancelled")))))

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

(defun bic-mailbox-update ()
  "Fetch new messages in current mailbox from server."
  (interactive)
  (unless (derived-mode-p 'bic-mailbox-mode)
    (user-error "Not a mailbox buffer"))
  (fsm-send (bic--find-account bic--current-account)
	    `(:ensure-up-to-date ,bic--current-mailbox)))

(defun bic-mailbox-read-message (keep-unread)
  "Open the message under point, and mark it as read.
With prefix argument, don't mark message as read."
  (interactive "P")
  (unless (derived-mode-p 'bic-mailbox-mode)
    (user-error "Not a mailbox buffer"))
  (let ((msg (ewoc-data (ewoc-locate bic-mailbox--ewoc (point)))))
    (bic-message-display bic--current-account
			 bic--current-mailbox
			 msg)
    (unless keep-unread
      (bic-message-flag '("\\Seen") '()))))

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
       ;; TODO: respect existing restrictions, such as "only unread"
       (puthash
	full-uid (ewoc-enter-last bic-mailbox--ewoc full-uid)
	bic-mailbox--ewoc-nodes-table))
      (node
       (when (ewoc-location node)
	 (let ((old-point (point)))
	   ;; We use an integer instead of a marker, because we don't
	   ;; expect the size of the entry to change, and if point was
	   ;; on this entry or immediately after it, we would lose the
	   ;; precise position and instead go back to the start of the
	   ;; entry if we used a marker.
	   (unwind-protect
	       (ewoc-invalidate bic-mailbox--ewoc node)
	     (goto-char old-point))))))))

(defun bic-mailbox--maybe-remove-message (address mailbox full-uid)
  (pcase (bic-mailbox--find-buffer address mailbox)
    ((and (pred bufferp) mailbox-buffer)
     (run-with-idle-timer
      0.1 nil 'bic-mailbox--remove-message mailbox-buffer full-uid))))

(defun bic-mailbox--remove-message (buffer full-uid)
  (with-current-buffer buffer
    (pcase (gethash full-uid bic-mailbox--ewoc-nodes-table)
      (`nil
       ;; Not found; nothing to do.
       )
      (node
       (when (ewoc-location node)
	 (let ((inhibit-read-only t))
	   (ewoc-delete bic-mailbox--ewoc node)))))))

;;; Message view

(defvar-local bic-message--full-uid nil
  "String containing uidvalidity and uid for message displayed in buffer.")

(defvar bic-message-mode-map
  ;; gnus-article-mode-map sets widget-keymap as its parent.  I'm not
  ;; yet ready to give up special-mode-map, so let's start out with a
  ;; copy of widget-keymap instead.  This lets us tab between elements
  ;; and hit RET to activate links etc.
  (let ((map (copy-keymap widget-keymap)))
    (set-keymap-parent map special-mode-map)
    ;; XXX: mark as replied, insert body, etc
    (define-key map "r" 'bic-message-reply)
    (define-key map "f" 'bic-message-wide-reply)
    (define-key map (kbd "C-c C-f") 'bic-message-forward)
    (define-key map "d" 'bic-message-mark-read)
    (define-key map (kbd "M-u") 'bic-message-mark-unread)
    (define-key map "!" 'bic-message-mark-flagged)
    (define-key map (kbd "B DEL") 'bic-message-mark-deleted)
    (define-key map "$" 'bic-message-mark-spam)
    (define-key map "\M-$" 'bic-message-mark-not-spam)
    ;; (define-key map (kbd "RET") 'bic-mailbox-read-message)
    (define-key map "t" 'bic-message-toggle-header)
    (define-key map "W" 'gnus-summary-wash-map)
    map))

(define-derived-mode bic-message-mode gnus-article-mode "BIC Message"
  "Major mode for messages viewed from `bic'.

Useful key bindings:
\\<bic-message-mode-map>
key\taction
---\t-------
\\[bic-message-mark-read]\tMark read
\\[bic-message-mark-unread]\tMark unread
\\[bic-message-mark-flagged]\tMark as \"flagged\"

\\[bic-message-reply]\tReply
\\[bic-message-wide-reply]\tReply all
\\[bic-message-forward]\tForward

\\[bic-message-toggle-header]\tToggle displaying full message headers

All key bindings:

\\{bic-message-mode-map}")

(defun bic-message-display (account mailbox msg)
  (with-current-buffer (get-buffer-create "*BIC-Message*")
    (let ((inhibit-read-only t))
      (bic-message-mode)
      ;; Don't ask :(
      (setq-local gnus-summary-buffer
		  (or (bic-mailbox--find-buffer account mailbox)
		      (current-buffer)))
      (setq-local gnus-article-buffer (current-buffer))
      (setq bic--current-account account
	    bic--current-mailbox mailbox
	    bic-message--full-uid msg
	    bic--dir (expand-file-name
		      ;; TODO: use bic--mailbox-dir
		      (bic--sanitize-mailbox-name mailbox)
		      (expand-file-name
		       account bic-data-directory)))
      ;; Keep the original text of the message in a separate buffer.
      (let ((dir bic--dir))
	(with-current-buffer (get-buffer-create gnus-original-article-buffer)
	  (erase-buffer)
	  (remove-overlays)
	  (insert-file-contents-literally
	   (expand-file-name msg dir) nil nil nil t)
	  (decode-coding-region (point-min) (point-max) 'raw-text-dos)
	  ;; "Original" but still decoded.
	  (run-hooks 'gnus-article-decode-hook)))
      (erase-buffer)
      (remove-overlays)
      ;; NB: gnus-article-mode gets very confused by CRLF line endings.
      ;; TODO: be more clever about what we decode.  Binary attachments?
      (insert-file-contents-literally
       (expand-file-name msg bic--dir) nil nil nil t)
      (decode-coding-region (point-min) (point-max) 'raw-text-dos)
      ;; Gnus already does a fine job displaying messages, so we might
      ;; as well piggy-back on that:
      (run-hooks 'gnus-article-decode-hook)
      (gnus-article-prepare-display))
    (let ((window (display-buffer (current-buffer))))
      (set-window-start window (point-min)))))

(defun bic-message-toggle-header (&optional arg)
  "Show the headers if they are hidden, or hide them if they are shown.
If ARG is a positive number, show the entire header.
If ARG is a negative number, hide the unwanted header lines."
  (interactive "P")
  (cl-letf (((symbol-function 'gnus-set-mode-line) #'ignore))
    (gnus-summary-toggle-header arg)))

(defun bic-message-mark-read ()
  "Mark the message at point as read.
If the message is marked as flagged, remove the flag.
If the message is marked to be deleted, undelete it."
  (interactive)
  (bic-message-flag '("\\Seen") '("\\Flagged" "\\Deleted")))

(defun bic-message-mark-unread ()
  "Mark the message at point as unread.
If the message is marked as flagged, remove the flag.
If the message is marked to be deleted, undelete it."
  (interactive)
  (bic-message-flag () '("\\Seen" "\\Flagged" "\\Deleted")))

(defun bic-message-mark-flagged ()
  "Mark the message at point as flagged.
Also mark it as read.
If the message is marked to be deleted, undelete it."
  (interactive)
  (bic-message-flag '("\\Seen" "\\Flagged") '("\\Deleted")))

(defun bic-message-mark-spam ()
  "Mark the message at point as spam (junk)."
  (interactive)
  (bic-message-flag '("$Junk") '("$NotJunk")))

(defun bic-message-mark-not-spam ()
  "Mark the message at point as not spam (not junk)."
  (interactive)
  (bic-message-flag '("$NotJunk") '("$Junk")))

(defun bic-message-mark-deleted ()
  "Mark the message at point for deletion."
  (interactive)
  (bic-message-flag '("\\Deleted") ()))

(defun bic-message-flag (flags-to-add flags-to-remove)
  "Add and remove flags for the message at point."
  (let ((full-uid (bic--find-message-at-point))
	(fsm (bic--find-account bic--current-account)))
    (fsm-send
     fsm
     (list :flags bic--current-mailbox full-uid flags-to-add flags-to-remove))))

(defun bic-message-reply (&optional wide)
  "Compose a reply to the current message."
  (interactive)
  (unless (derived-mode-p 'bic-message-mode)
    (user-error "Not in message buffer"))
  (let ((full-uid (bic--find-message-at-point))
	(mailbox bic--current-mailbox)
	(account bic--current-account))
    (set-buffer gnus-original-article-buffer)
    (message-reply nil wide)
    (add-to-list
     'message-send-actions
     (lambda ()
       (fsm-send
	(bic--find-account account)
	(list :flags mailbox full-uid '("\\Answered") ()))))))

(defun bic-message-wide-reply ()
  "Compose a wide reply (\"reply all\") to the current message."
  (interactive)
  (bic-message-reply t))

(defun bic-message-forward ()
  "Forward the current message."
  (interactive)
  (unless (derived-mode-p 'bic-message-mode)
    (user-error "Not in message buffer"))
  (let ((full-uid (bic--find-message-at-point))
	(mailbox bic--current-mailbox)
	(account bic--current-account))
    (set-buffer gnus-original-article-buffer)
    (message-forward)
    (add-to-list
     'message-send-actions
     (lambda ()
       (fsm-send
	(bic--find-account account)
	(list :flags mailbox full-uid '("$Forwarded") ()))))))

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
