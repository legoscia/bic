;;; bic.el --- Best IMAP Client                      -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Magnus Henoch

;; Author: Magnus Henoch <magnus.henoch@gmail.com>
;; Keywords: mail
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "25"))

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
(require 'gnus)
(require 'gnus-art)
(require 'gnus-range)
(require 'gnus-srvr)
(require 'hex-util)
(require 'avl-tree)

(declare-function bic-mailbox-tree "bic-mailbox-tree")

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

(defvar bic-command-timeout 30
  "Time in seconds allowed for server to respond to commands.
If no data has been received during this time, the connection is
considered dead.")

(defvar-local bic--current-account nil)

(defvar-local bic--current-mailbox nil)

(defvar-local bic--dir nil)

(defconst bic--pending-flags-prefixes '("+" "-")
  "Possible markers in pending flags files.")

(defconst bic--pending-flags-prefixes-regexp
  (regexp-opt bic--pending-flags-prefixes))

;;;###autoload
(defun bic (&optional new-account)
  "Start BIC.
If there are no configured accounts, or if a prefix argument is
given (setting NEW-ACCOUNT to non-nil), prompt for email address.
Otherwise, start BIC for all known addresses."
  (interactive "P")
  (let ((accounts (and (file-directory-p bic-data-directory)
		       (directory-files bic-data-directory nil "@"))))
    (if (or new-account (null accounts))
	(call-interactively #'bic-add-account)
      (mapc #'bic-add-account
	    (cl-remove-if #'bic--find-account accounts))
      (bic-mailbox-tree))))

;;;###autoload
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

(defun bic-deactivate-all ()
  "Temporarily deactivate all accounts.
Close any existing connections, and don't attempt to reconnect
until reactivated with `bic-activate' or `bic-activate-all'."
  (interactive)
  (dolist (a bic-running-accounts)
    (fsm-send a :deactivate)))

(defun bic-activate (account)
  "Reactivate ACCOUNT.
Attempt to reconnect to an account previously disabled with
`bic-deactivate'."
  (interactive (list (bic--read-running-account)))
  (fsm-send (bic--find-account account) :activate))

(defun bic-activate-all ()
  "Reactivate all accounts.
Attempt to reconnect any accounts previously disabled with
`bic-deactivate' or `bic-deactivate-all'."
  (interactive)
  (dolist (a bic-running-accounts)
    (fsm-send a :activate)))

(defun bic-stop (account)
  "Stop the BIC state machine for ACCOUNT.

If you want to keep using BIC, but stop it from attempting to
reconnect to a certain account, use `bic-deactivate' instead."
  (interactive (list (bic--read-running-account)))
  (fsm-send (bic--find-account account) :stop))

(defun bic-stop-all ()
  "Stop the BIC state machines for all accounts.
See `bic-stop'."
  (interactive)
  (dolist (a bic-running-accounts)
    (fsm-send a :stop)))

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
    (plist-put state-data :username (bic--ask-for-username address))
    (fsm-send fsm :try-connect)
    (list state-data nil)))

(defun bic--ask-for-username (address)
  (let ((user-part (substring address 0 (cl-position ?@ address))))
    (pcase (widget-choose
	    "IMAP username"
	    (list (cons (concat "Authenticate as " address) address)
		  (cons (concat "Authenticate as " user-part) user-part)
		  (cons "Use a different username" :other)))
      ((and (pred stringp) username)
       username)
      (`:other
       (read-string "Enter IMAP username: ")))))

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
		   :auth-wait
		   (apply-partially #'bic--untagged-callback fsm))
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
	     (fsm-send fsm (list status connection)))
	   nil
	   (apply-partially #'bic--untagged-callback fsm))))
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
  (_fsm state-data event callback)
  (let ((our-connection (plist-get state-data :connection)))
    (pcase event
      (`((:disconnected ,keyword ,reason) ,(pred (eq our-connection)))
       ;; We used to display a message here, but that can be rather
       ;; annoying if you're starting BIC without a network
       ;; connection.  Just check the mailbox tree view to see if
       ;; you're connected or not.
       (list :disconnected state-data))
      (:timeout
       ;; Ditto.
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
		   #'ignore
		   '((0 "ID" ignore))))

    (when (bic-connection--has-capability "NOTIFY" c)
      (bic-command
       c
       (concat
	"NOTIFY SET (selected-delayed (MessageNew MessageExpunge FlagChange))"
	" (subscribed (MessageNew MessageExpunge FlagChange))"
	" (personal (MailboxName SubscriptionChange))")
       (lambda (notify-response)
	 (pcase notify-response
	   (`(,(or :no :bad) ,response ,_response-lines)
	    (warn "Cannot enable NOTIFY for %s: %s"
		  (plist-get state-data :address)
		  (plist-get response :text)))))))

    ;; Get list of mailboxes
    (bic-command c
		 (cond
		  ((bic-connection--has-capability "LIST-EXTENDED" c)
		   (concat
		    "LIST \"\" \"*\" RETURN (SUBSCRIBED"
		    (when (bic-connection--has-capability "LIST-STATUS" c)
		      (concat " STATUS ("
			      (bic--interesting-status-items c)
			      ")"))
		    (when (bic-connection--has-capability "SPECIAL-USE" c)
		      " SPECIAL-USE")
		    ")"))
		  (t
		   "LIST \"\" \"*\""))
		 (lambda (response)
		   (fsm-send fsm (list :list-response response)))
		 '((0 "LIST" :keep)
		   (0 "STATUS" :keep))))
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
	  (`(,(pred (string= mailbox-name)) . ,_)
	   (plist-put state-data :current-task nil)
	   (bic--maybe-next-task fsm state-data)))
	(list :connected state-data nil))))
    (`(:untagged-response ,untagged-response)
     ;; XXX: check :selecting and :selected
     (let ((selecting (plist-get state-data :selecting))
	   (selected (plist-get state-data :selected)))
       (pcase untagged-response
	 (`("STATUS" ,mailbox-name ,status-att-list)
	  (bic--handle-status-response
	   fsm state-data mailbox-name status-att-list
	   :queue-sync-tasks t)
	  (list :connected state-data nil))
	 ((guard selecting)
	  (warn "Cannot handle untagged response %S: is it for '%s' or '%s'?"
		untagged-response selecting selected)
	  nil)
	 (`(,seq-no "EXPUNGE")
	  ;; A somewhat complicated dance to ensure that after receiving
	  ;; EXPUNGE responses, we'll exit IDLE, and run the expunge task
	  ;; once and only once, with all received message sequence
	  ;; numbers.  Thus we build the task using `nconc', and add
	  ;; it with `bic--queue-task-if-new', which skips duplicate
	  ;; tasks.
	  (let ((expunge-task (plist-get state-data :expunge-task)))
	    (when (or (null expunge-task)
		      (not (string= (car expunge-task) selected)))
	      (setq expunge-task (list selected :expunge-messages))
	      (plist-put state-data :expunge-task expunge-task))
	    (nconc expunge-task (list seq-no))
	    (bic--queue-task-if-new state-data expunge-task)
	    (bic--maybe-next-task fsm state-data)
	    ;; TODO: decrement "EXISTS" value for current mailbox
	    (list :connected state-data nil)))
	 (`(,_ "RECENT")
	  ;; We don't really care about the \Recent flag.  Assuming
	  ;; that the server always sends EXISTS along with RECENT,
	  ;; we can ignore this.
	  (list :connected state-data nil))
	 (`(,how-many "EXISTS")
	  (plist-put (cdr (assoc selected (plist-get state-data :mailboxes)))
		     :exists (string-to-number how-many))
	  ;; TODO: just fetch new messages, as we know their
	  ;; sequence numbers.
	  (fsm-send
	   fsm
	   `(:queue-task
	     (,selected :sync-mailbox
		       ,@(unless (eq :unlimited-sync
				     (bic--mailbox-sync-level state-data selected))
			   '(:limit 100)))))
	  (list :connected state-data nil))
	 (`(,_ "FETCH" ,_msg-att)
	  ;; XXX: this is ugly. In bic--handle-fetch-response, we
	  ;; check the UIDVALIDITY value of the mailbox, but here we
	  ;; don't have any reliable value, so we just fetch the
	  ;; stored value, rendering the check meaningless.
	  (let* ((mailbox-plist (cdr (assoc selected (plist-get state-data :mailboxes))))
		 (mailbox-uidvalidity (plist-get mailbox-plist :uidvalidity)))
	    (bic--handle-fetch-response state-data selected untagged-response mailbox-uidvalidity))
	  (list :connected state-data nil))
	 (_
	  (warn "Unexpected untagged response %S" untagged-response)
	  nil))))
    (`(:early-fetch-response ,selected-mailbox ,msg ,uidvalidity)
     (bic--handle-fetch-response state-data selected-mailbox msg uidvalidity)
     (list :connected state-data))
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
       (bic--cancel-command-timeout-timer state-data)
       (bic--maybe-next-task fsm state-data))
     (list :connected state-data))
    (`(:flags ,mailbox ,full-uid ,flags-to-add ,flags-to-remove)
     (bic--write-pending-flags mailbox full-uid flags-to-add flags-to-remove state-data)
     (bic--queue-task-if-new state-data (list mailbox :pending-flags))
     (bic--maybe-next-task fsm state-data)
     (list :connected state-data))
    (`(:copy ,from-mailbox ,to-mailbox ,full-uid)
     ;; TODO: offline-friendly
     (bic--queue-task-if-new state-data (list from-mailbox :copy to-mailbox full-uid))
     (list :connected state-data))
    (`(:sync-level ,mailbox ,new-sync-level)
     ;; Update sync level and maybe send subscribe command.
     (when (bic--set-sync-level state-data mailbox new-sync-level)
       (bic--queue-task-if-new
	state-data
	(list :any-mailbox :subscribe mailbox))
       (bic--maybe-next-task fsm state-data))
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
    (`(:command-timeout ,command-timeout-gensym ,time)
     (cond
      ((not (eq command-timeout-gensym (plist-get state-data :command-timeout-gensym)))
       ;; This timeout event doesn't correspond to the command we're
       ;; currently executing.  Ignore it.
       (list :connected state-data))
      ((time-less-p time
		    (bic-connection--latest-received (plist-get state-data :connection)))
       ;; The connection has received data since this timer was
       ;; created, so we know for sure that it was alive not long ago.
       ;; Start a new timer.
       (bic--start-command-timeout-timer fsm state-data)
       (list :connected state-data))
      (t
       ;; We've sent a command to the server, and waited
       ;; `bic-command-timeout' seconds, but we haven't received a
       ;; single byte from the server.  Probably the connection is
       ;; dead without the OS telling us that.
       (message "Timeout while waiting for data; connection for %s considered lost"
		(plist-get state-data :address))
       (fsm-send (plist-get state-data :connection) :stop)
       (list :disconnected state-data))))
    (`(:ensure-up-to-date ,mailbox . ,options)
     (bic--queue-task-if-new
      state-data
      `(,mailbox :sync-mailbox
		 ,@(unless (eq :unlimited-sync (bic--mailbox-sync-level state-data mailbox))
		     '(:limit 100))
		 :verbose ,(plist-get options :verbose)))
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

(defun bic--untagged-callback (fsm untagged-response)
  (fsm-send fsm (list :untagged-response untagged-response)))

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
	 (status-return
	  (bic-connection--has-capability "LIST-STATUS" (plist-get state-data :connection)))
	 mailboxes status-responses)
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
	;; Need to buffer STATUS responses here, as we haven't
	;; populated the mailbox list yet.
	(`("STATUS" ,_mailbox-name ,_status-att-list)
	 (push x status-responses))
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
    ;; Take STATUS responses into account
    (dolist (status-response status-responses)
      (bic--handle-status-response
       fsm state-data (cl-second status-response) (cl-third status-response)
       ;; Don't sync just yet.
       :queue-sync-tasks nil))
    (if subscription-return
	;; If we have subscription info, we can sync mailboxes.
	(if status-return
	    (bic--queue-task-if-new state-data (list :any-mailbox :sync-mailboxes))
	  ;; But if we didn't get STATUS here, let's get that first.
	  (bic--queue-task-if-new state-data (list :any-mailbox :list-status-all)))
      ;; If we didn't get the subscription info already, ask for it with LSUB.
      (bic-command
       (plist-get state-data :connection)
       "LSUB \"\" \"*\""
       (lambda (response)
    	 (fsm-send fsm (list :lsub-response response)))
       '((0 "LSUB" :keep))))))

(cl-defun bic--handle-status-response (fsm state-data mailbox-name status-att-list
					   &key queue-sync-tasks)
  (let* ((mailboxes (plist-get state-data :mailboxes))
	 (mailbox-entry (assoc mailbox-name mailboxes)))
    (if (null mailbox-entry)
	(warn "STATUS response for unknown mailbox %S" mailbox-name)
      (pcase (member "UIDVALIDITY" status-att-list)
	(`("UIDVALIDITY" ,uidvalidity . ,_)
	 (setf (cdr mailbox-entry)
	       (plist-put (cdr mailbox-entry) :uidvalidity uidvalidity))))
      (pcase (member "HIGHESTMODSEQ" status-att-list)
	(`("HIGHESTMODSEQ" ,highest-modseq . ,_)
	 (setf (cdr mailbox-entry)
	       (plist-put (cdr mailbox-entry) :server-modseq highest-modseq))))
      ;; Only remember unseen count for mailboxes being synced,
      ;; since for the others we don't make any effort to keep
      ;; it up to date.
      (when (bic--infer-sync-level mailbox-entry)
	(pcase (member "UNSEEN" status-att-list)
	  (`("UNSEEN" ,unseen . ,_)
	   (setf (cdr mailbox-entry)
		 (plist-put (cdr mailbox-entry) :unseen (string-to-number unseen)))
	   (bic--update-mailbox-status (plist-get state-data :address)
				       (car mailbox-entry) (cdr mailbox-entry)))))
      ;; TODO: check MESSAGES, UIDNEXT etc for empty mailboxes: no need
      ;; to sync them.
      (when queue-sync-tasks
	(pcase (bic--mailbox-sync-task-maybe state-data mailbox-entry)
	  (`(,task)
	   (bic--queue-task-if-new state-data task)
	   (bic--maybe-next-task fsm state-data)))))))

(defun bic--handle-fetch-response (state-data selected-mailbox msg uidvalidity)
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
	       (bic--print-sexp (bic-expand-literals new-flags))
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
		(bic--print-sexp envelope-data)
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
    ))

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
    (if (bic-connection--has-capability "LIST-STATUS" (plist-get state-data :connection))
	(bic--queue-task-if-new state-data (list :any-mailbox :sync-mailboxes))
      ;; But get STATUS first, if we couldn't before.
      (bic--queue-task-if-new state-data (list :any-mailbox :list-status-all)))))

(defun bic--sync-mailboxes (fsm state-data task)
  ;; TODO: do we need to issue a new LIST-STATUS request here?
  ;; XXX: would that overwrite everything we know?
  (let ((download-messages-tasks
	 (cl-mapcan
	  (lambda (mailbox-data)
	    (bic--mailbox-sync-task-maybe state-data mailbox-data))
	  (plist-get state-data :mailboxes))))
    (plist-put state-data :tasks
	       (append (plist-get state-data :tasks)
		       download-messages-tasks))
    (fsm-send fsm (list :task-finished task))))

(defun bic--mailbox-sync-task-maybe (state-data mailbox-data)
  "Check whether to sync a mailbox.
If sync needed, return a list of one task.
If no sync needed, return an empty list."
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
      (let ((task (bic--mailbox-sync-task state-data (car mailbox-data))))
	(when task
	  (list task))))))

(defun bic--mailbox-sync-task (state-data mailbox-name)
  "Return the appropriate sync task for MAILBOX-NAME.
Return nil if mailbox should not be synced."
  (cl-case (bic--mailbox-sync-level state-data mailbox-name)
    (:unlimited-sync
     (list mailbox-name :sync-mailbox))
    (:partial-sync
     (list mailbox-name :sync-mailbox :limit 100))))

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
	"want-subscribed"
	(bic--mailbox-dir state-data mailbox))
       nil :silent)
      t)))

(defun bic--mailbox-sync-level (state-data mailbox)
  (let ((mailbox-data (assoc mailbox (plist-get state-data :mailboxes))))
    (bic--infer-sync-level mailbox-data)))

(defun bic--infer-sync-level (mailbox-data)
  ;; Accept either a raw plist, or a plist prefixed with a mailbox name.
  (when (stringp (car mailbox-data))
    (setq mailbox-data (cdr mailbox-data)))
  (let ((attributes (plist-get mailbox-data :attributes))
	(explicit-sync-level (plist-get mailbox-data :explicit-sync-level)))
    (cond
     ((or (cl-member "\\Noselect" attributes :test #'cl-equalp)
	  (cl-member "\\NonExistent" attributes :test #'cl-equalp))
      nil)
     (explicit-sync-level
      (if (eq explicit-sync-level :no-sync)
	  nil
	explicit-sync-level))
     ((and (cl-member "\\Subscribed" attributes :test #'cl-equalp)
	   ;; If this is a virtual "all mail" mailbox, don't
	   ;; sync it implicitly.
	   (not (cl-member "\\All" attributes :test #'cl-equalp)))
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
     0 nil
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
	     ((and highestmodseq-entry
		   (bic-connection--has-capability
		    "CONDSTORE" (plist-get state-data :connection)))
	      (let* ((old-modseq-string
		      (when (file-exists-p modseq-file)
			(with-temp-buffer
			  (insert-file-contents-literally modseq-file)
			  (buffer-string))))
		     (old-modseq-pair (and old-modseq-string (split-string old-modseq-string "-")))
		     (new-modseq (plist-get (cdr highestmodseq-entry) :data)))
		(plist-put (cdr mailbox-entry) :our-modseq
			   (when (and old-modseq-string
				      (string= (car old-modseq-pair) uidvalidity))
			     (cadr old-modseq-pair)))
		(plist-put (cdr mailbox-entry) :server-modseq new-modseq)
		(unless (string= (cadr old-modseq-pair) new-modseq)
		  ;; The modseq differs.  If the mailbox is to be
		  ;; synced, schedule a sync now.
		  (bic--queue-task-if-new state-data
					  (bic--mailbox-sync-task state-data mailbox-name)))))
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
	(warn "Missing UIDVALIDITY for `%s' (%s)!  This is not good."
	      mailbox-name (plist-get state-data :address))))

    (plist-put state-data :selected mailbox-name)))

(defun bic--queue-task-if-new (state-data task)
  (unless (or (null task) (equal task (plist-get state-data :current-task)))
    (let ((existing-tasks (plist-get state-data :tasks)))
      (unless (member task existing-tasks)
	(plist-put state-data :tasks
		   (append existing-tasks (list task)))))))

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
    ;; Start a timer, so that we detect that the connection is dead.
    (bic--start-command-timeout-timer fsm state-data)
    (bic-command
     c
     (concat "SELECT " (bic-quote-string mailbox)
	     (when (bic-connection--has-capability "CONDSTORE" c)
	       " (CONDSTORE)"))
     (lambda (select-response)
       (fsm-send fsm (list :select-response mailbox select-response)))
     ;; XXX: more :keep "callbacks"?
     '((0 :ok :keep)
       (0 "FLAGS" :keep)
       (1 "EXISTS" :keep)
       (1 "RECENT" ignore)))))

(defun bic--start-command-timeout-timer (fsm state-data)
  "Start a new command timeout timer.
Cancel existing timer, if any.

If no data has been received for `bic-command-timeout' seconds,
consider the connection dead."
  (let* ((command-timeout-gensym (cl-gensym "command-timeout-"))
	 (time (current-time))
	 (timer (run-with-timer
		 bic-command-timeout nil
		 (lambda ()
		   ;; Give this connection a chance to have its data
		   ;; processed, in case some other connection has
		   ;; been hogging all the attention.
		   (bic-connection--accept-output
		    (plist-get state-data :connection))
		   (fsm-send fsm (list :command-timeout
				       command-timeout-gensym
				       time))))))
    (bic--cancel-command-timeout-timer state-data)
    (plist-put state-data :command-timeout-gensym command-timeout-gensym)
    (plist-put state-data :command-timeout-timer timer)))

(defun bic--cancel-command-timeout-timer (state-data)
  (let ((previous-timer (plist-get state-data :command-timeout-timer)))
    (when (timerp previous-timer)
      (cancel-timer previous-timer))
    (plist-put state-data :command-timeout-gensym nil)))

(defun bic--do-task (fsm state-data task)
  ;; No matter what we do, setting a timeout for it is a good idea.
  (bic--start-command-timeout-timer fsm state-data)
  (pcase task
    (`(,mailbox :pending-flags)
     (bic--apply-pending-flags fsm state-data task mailbox))
    (`(,mailbox :sync-mailbox . ,_options)
     ;; At this point, we should have selected the mailbox already.
     (cl-assert (string= mailbox (plist-get state-data :selected)))
     (bic--sync-mailbox fsm state-data task))

    (`(,mailbox :expunge-messages . ,sequence-numbers)
     (cl-assert (string= mailbox (plist-get state-data :selected)))
     (when (eq task (plist-get state-data :expunge-task))
       (plist-put state-data :expunge-task nil))
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
	;; TODO: use ESEARCH if available?
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
			       (car (last sorted))))
		    expunged-full-uids)
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
		     (push (concat mailbox-uidvalidity "-" (bic-number-to-string uid))
			   expunged-full-uids)))
	       (bic--messages-expunged state-data mailbox expunged-full-uids)))
	    (search-error
	     (warn "Error in response to SEARCH: %S" search-error)))
	  (fsm-send fsm (list :task-finished task)))
	'((0 "SEARCH" :keep)
	  (0 "ESEARCH" :keep)))))
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
    (`(:any-mailbox :list-status-all)
     (let ((c (plist-get state-data :connection)))
       (if (and (bic-connection--has-capability "LIST-EXTENDED" c)
		(bic-connection--has-capability "LIST-STATUS" c))
	   (bic-command
	    c
	    (concat "LIST (SUBSCRIBED) \"\" \"*\" RETURN (STATUS ("
		    (bic--interesting-status-items c)
		    "))")
	    (lambda (response)
	      (unless (eq (car response) :ok)
		(warn "LIST request failed: %S" response))
	      (fsm-send fsm (list :task-finished task)))
	    '((0 "LIST" ignore)))
	 ;; LIST-STATUS not supported.  Send STATUS requests for each mailbox.
	 (let* ((interesting-mailboxes
		 (cl-remove-if-not #'bic--infer-sync-level (plist-get state-data :mailboxes)))
		(remaining (length interesting-mailboxes)))
	   ;; If there are no "interesting" mailboxes, we're done.
	   (if (zerop remaining)
	       (fsm-send fsm (list :task-finished task))
	     (dolist (mailbox-data interesting-mailboxes)
	       (when (bic--infer-sync-level mailbox-data)
		 (bic-command
		  c
		  (concat "STATUS " (bic-quote-string (car mailbox-data))
			  " ("
			  (bic--interesting-status-items c)
			  ")")
		  (lambda (_response)
		    (when (zerop (cl-decf remaining))
		      (fsm-send fsm (list :task-finished task))))))))))))
    (`(,_ :logout)
     ;; No need for a callback - this is the last task.
     (bic-command (plist-get state-data :connection) "LOGOUT" #'ignore))
    (_
     (warn "Unknown task %S" task))))

(defun bic--apply-pending-flags (fsm state-data task mailbox)
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
		   t))
		 (returned-uids nil))
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

		    ;; If we didn't get a FETCH response for some of
		    ;; the messages, perhaps they have already been
		    ;; deleted.  Let's do a SEARCH for those UIDs just
		    ;; to be sure, and consider those we don't get
		    ;; back to have been deleted.
		    (let ((missing-uids (gnus-remove-from-range ranges returned-uids)))
		      (when missing-uids
			(bic-command
			 c
			 (concat "UID SEARCH UID "
				 (bic-format-ranges missing-uids))
			 (lambda (search-response)
			   (pcase search-response
			     (`(:ok ,_ ,search-data)
			      (let* ((search-results
				      (mapcar #'string-to-number
					      (cdr (assoc "SEARCH" search-data))))
				     (still-missing (gnus-remove-from-range
						     missing-uids
						     search-results)))
				(bic--messages-expunged
				 state-data mailbox
				 (mapcar (lambda (uid)
					   (concat mailbox-uidvalidity "-"
						   (bic-number-to-string uid)))
					 still-missing))))))
			 '((0 "SEARCH" :keep)))))

		    ;; If we added the \Deleted flag, also expunge:
		    (pcase (gethash '("+" . "\\Deleted") flag-change-uids)
		      (`nil
		       ;; Nothing to expunge, task finished.
		       (fsm-send fsm (list :task-finished task)))
		      (deleted-uids
		       (let ((expunge-command
			      (if (bic-connection--has-capability "UIDPLUS" c)
				  ;; We can use UID EXPUNGE to expunge
				  ;; only the affected messages.
				  (concat
				   "UID EXPUNGE "
				   (bic-format-ranges
				    (gnus-compress-sequence
				     (sort (mapcar #'string-to-number deleted-uids)
					   #'<)
				     t)))
				;; The server doesn't support UIDPLUS.
				;; Let's just expunge everything
				;; that's marked \Deleted, like Gnus
				;; does.
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
		       ;; Remember which UIDs we've seen FETCH
		       ;; responses for.
		       (pcase one-fetch-response
			 (`(,_seq "FETCH" ,msg-att)
			  (let* ((uid-entry (member "UID" msg-att))
				 (uid (cadr uid-entry)))
			    (when uid
			      (push (string-to-number uid) returned-uids)))))
		       (fsm-send fsm (list :early-fetch-response
					   mailbox
					   one-fetch-response
					   mailbox-uidvalidity))))))))
	 flag-change-uids)))))

(defun bic--interesting-status-items (c)
  (concat "MESSAGES UIDNEXT UNSEEN UIDVALIDITY"
	  (when (bic-connection--has-capability "CONDSTORE" c)
	    " HIGHESTMODSEQ")))

(defun bic--idle (fsm state-data)
  (let* ((idle-gensym (cl-gensym "IDLE-"))
	 (timer (run-with-timer (* 29 60) nil
				(lambda ()
				  (fsm-send fsm (list :idle-timeout idle-gensym))))))
    ;; In idle mode, we use an idle timer, so cancel any existing
    ;; command timer.
    (bic--cancel-command-timeout-timer state-data)
    (plist-put state-data :current-task (list :idle idle-gensym timer))
    (bic-command
     (plist-get state-data :connection)
     "IDLE"
     (lambda (idle-response)
       (fsm-send fsm (list :idle-response idle-response idle-gensym)))
     (list
      (list
       0 :ok
       ;; A periodic "OK" response from the server.  (Dovecot does this.)
       (lambda (ok-response)
	 (when (plist-get (cdr ok-response) :code)
	   (warn "Unknown response while IDLE: %S %S %S"
		 (plist-get (cdr ok-response) :code)
		 (plist-get (cdr ok-response) :data)
		 (plist-get (cdr ok-response) :text)))))
      (list 0 "FLAGS" #'ignore)))))

(defun bic--idle-done (fsm state-data)
  (let ((idle-gensym (cl-second (plist-get state-data :current-task))))
    (plist-put state-data :current-task
	       (list :idle-done idle-gensym
		     (run-with-timer
		      10 nil
		      (lambda ()
			(fsm-send fsm (list :idle-done-timeout idle-gensym)))))))
  (condition-case e
      (bic--send (plist-get state-data :connection) "DONE\r\n")
    (error
     (cond
      ((and (stringp (cdr e))
	    (string-match-p "no longer connected to pipe" (cdr e)))
       ;; The connection died.  That's an expected error.  The
       ;; sentinel should fire and take care of it.
       nil)
      (t
       ;; Different error.  Rethrow it.
       (signal (car e) (cdr e)))))))

(defun bic--numeric-string-lessp (s1 s2)
  (cond ((< (length s1) (length s2)) t)
	((> (length s1) (length s2)) nil)
	((string= s1 s2) nil)
	(t (string-lessp s1 s2))))

(defun bic--sync-mailbox (fsm state-data task)
  ;; TODO: do something clever with the limit, so we don't need to
  ;; dig through too much data.
  (let* ((mailbox (car task))
	 (connection (plist-get state-data :connection))
	 unseen-flagged-search-data recent-search-data
	 (mailbox-plist
	  (cdr (assoc mailbox (plist-get state-data :mailboxes))))
	 (uidvalidity (plist-get mailbox-plist :uidvalidity))
	 (our-modseq (plist-get mailbox-plist :our-modseq))
	 (server-modseq (plist-get mailbox-plist :server-modseq))
	 (highest-modseq our-modseq)
	 (overview-table (bic--read-overview state-data mailbox))
	 (uid-tree (gethash mailbox (plist-get state-data :uid-tree-per-mailbox)))
	 numeric-uids)

    ;; TODO: check for deleted messages.  Ideally support QRESYNC.
    ;; If the server does _not_ support CONDSTORE, mix in with flag
    ;; requests.  (Or is that a bad idea?)
    ;; If the server supports ESEARCH, use that.
    ;; Perhaps do something clever with EXISTS - which _may_ be
    ;; outdated at this point.
    (when (bic-connection--has-capability "ESEARCH" connection)
      ;; ESEARCH permits a relatively cheap way to check for some
      ;; deleted messages in the absence of QRESYNC - at least the
      ;; ones at the beginning or end of the list of messages we know
      ;; about.
      (let ((min (avl-tree-first uid-tree))
	    (max (avl-tree-last uid-tree)))
	(when (and min max)
	  (bic-command
	   connection
	   (format "UID SEARCH RETURN (MIN MAX) UID %s:%s"
		   (bic-number-to-string min) (bic-number-to-string max))
	   (lambda (search-response)
	     (pcase search-response
	       (`(:ok ,_ (("ESEARCH" ("TAG" ,_) "UID" . ,search-data)))
		(let ((min-string (cadr (member "MIN" search-data)))
		      (max-string (cadr (member "MAX" search-data))))
		  (if (and (null min-string) (null max-string))
		      ;; Special case: _all_ the messages we knew
		      ;; about have been deleted.
		      (bic--messages-expunged
		       state-data mailbox
		       (avl-tree-mapcar
			(lambda (uid) (concat uidvalidity "-" (bic-number-to-string uid)))
			uid-tree))
		    ;; If there is at least one message, we should
		    ;; have gotten non-nil MIN and MAX results as
		    ;; well.
		    (let ((actual-min (string-to-number min-string))
			  (actual-max (string-to-number max-string))
			  (expunged-count 0)
			  expunged-full-uids)
		      (when (> actual-min min)
			;; The messages below actual-min have been
			;; expunged on the server.  Loop through our
			;; UIDs, exiting once we've hit actual-min.
			(catch 'done
			  (avl-tree-mapc
			   (lambda (uid)
			     (if (>= uid actual-min)
				 (throw 'done t)
			       (push (concat uidvalidity "-" (bic-number-to-string uid))
				     expunged-full-uids)
			       (cl-incf expunged-count)))
			   uid-tree nil)))
		      (when (< actual-max max)
			;; The messages above actual-max have been
			;; expunged on the server.  Loop through our
			;; UIDs in reverse order, exiting once we've
			;; hit actual-max.
			(catch 'done
			  (avl-tree-mapc
			   (lambda (uid)
			     (if (<= uid actual-max)
				 (throw 'done t)
			       (push (concat uidvalidity "-" (bic-number-to-string uid))
				     expunged-full-uids)
			       (cl-incf expunged-count)))
			   uid-tree t)))
		      (bic--messages-expunged state-data mailbox expunged-full-uids)))))
	       (`(:ok ,_ ,other-search-data)
		(warn "Unexpected search result: %S" other-search-data))
	       (other
		(warn "Search failed: %S" other))))

	   '((0 "ESEARCH" :keep))))))

    (setq numeric-uids (avl-tree-flatten uid-tree))
    ;; We can skip requesting flags for already downloaded messages
    ;; if there aren't any downloaded messages, or if the server
    ;; supports CONDSTORE and the MODSEQ values match.
    (when (and (not (zerop (hash-table-count overview-table)))
	       (or (not (bic-connection--has-capability "CONDSTORE" connection))
		   (null server-modseq)
		   (null our-modseq)
		   (bic--numeric-string-lessp our-modseq server-modseq)))
      ;; XXX: if the mailbox has many messages, and the server
      ;; doesn't support CONDSTORE, this will take really long
      ;; time, as we unconditionally get the flag status of every
      ;; message we know about.
      (bic-uids-command
       connection
       "UID FETCH "
       (gnus-compress-sequence numeric-uids t)
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
	  (warn "SEARCH for unseen/flagged messages failed: %S" other))))
     '((0 "SEARCH" :keep)
       (0 "ESEARCH" :keep)))
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
	highest-modseq server-modseq))
     '((0 "SEARCH" :keep)
       (0 "ESEARCH" :keep)))))

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
	 (verbose (plist-get (cddr task) :verbose))
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

    (let
	((uid-overview
	  (lambda (uid)
	    (gethash (concat uidvalidity "-" (bic-number-to-string uid))
		     overview-table)))
	 (write-modseq
	  (lambda ()
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
	       modseq-file))))))
      ;; Now we know which messages we're not going to fetch because
      ;; they exceed the limit.  If they haven't been downloaded
      ;; already, warn about it.
      ;; (message "Skipping unseen/flagged: %S recent: %S"
      ;; 	       (cl-remove-if #'uid-overview not-these-unseen)
      ;; 	       (cl-remove-if #'uid-overview not-these-recent))
      (cond
       ((cl-member-if-not uid-overview (gnus-uncompress-range not-these-unseen))
	(unless (plist-get mailbox-entry :not-all-unread)
	  (message "%s (%s) has too many new/flagged messages!  Limiting to the latest %d"
		mailbox (plist-get state-data :address) limit)
	  ;; TODO: set a permanent flag
	  (bic--update-mailbox-status
	   (plist-get state-data :address)
	   mailbox
	   (plist-put mailbox-entry :not-all-unread t))))
       ((cl-member-if-not uid-overview (gnus-uncompress-range not-these-recent))
	(unless (plist-get mailbox-entry :not-all-recent)
	  ;; TODO: improve wording
	  (message "%s (%s) has too many recent messages!  Limiting to the latest %d"
		mailbox (plist-get state-data :address) limit)
	  ;; TODO: set a permanent flag
	  (bic--update-mailbox-status
	   (plist-get state-data :address)
	   mailbox
	   (plist-put mailbox-entry :not-all-recent t)))))

      ;; Don't fetch messages we've already downloaded.
      ;; TODO: is it possible that we have the envelope, but not the body?
      (pcase (cl-delete-if uid-overview (gnus-uncompress-range fetch-these))
	(`nil
	 (funcall write-modseq)
	 (when verbose
	   (message "Nothing to fetch from %s for %s"
		    mailbox (plist-get state-data :address)))
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
		 (funcall write-modseq))
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
		    (concat "^\\([0-9]+-[0-9]+\\)\\("
			    bic--pending-flags-prefixes-regexp
			    "\\)\\(.*\\)$")
		    nil t)
	      (let* ((full-uid (match-string 1))
		     (add-remove (match-string 2))
		     (flag (match-string 3))
		     (existing-flags (gethash full-uid flags-table))
		     (new-flags
		      (pcase add-remove
			("+" (cl-union existing-flags (list flag) :test 'string=))
			("-" (remove flag existing-flags))
			(_   existing-flags))))
		(puthash full-uid (cl-adjoin :pending new-flags) flags-table)))))))
    flags-table))

(defun bic--write-pending-flags (mailbox full-uid flags-to-add flags-to-remove state-data)
  (pcase-let ((`(,uidvalidity ,_uid) (split-string full-uid "-"))
	      ;; XXX: get mailbox data here
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
	;; XXX: check for \Seen in need-to-add and need-to-remove, and adjust counter
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

(defun bic--messages-expunged (state-data mailbox expunged-full-uids)
  (let* ((overview-table (bic--read-overview state-data mailbox))
	 (uid-tree (gethash mailbox (plist-get state-data :uid-tree-per-mailbox)))
	 (dir (bic--mailbox-dir state-data mailbox))
	 (overview-file (expand-file-name "overview" dir)))
    (dolist (full-uid expunged-full-uids)
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
	(avl-tree-delete uid-tree
			 (string-to-number (substring full-uid (1+ (cl-position ?- full-uid)))))
	(bic-mailbox--maybe-remove-message
	 (plist-get state-data :address)
	 mailbox full-uid)
	;; TODO: remove file on disk once this works properly
	))))

(defun bic--date-text (time)
  (pcase-let ((`(,_sec ,_min ,_hour ,day ,month ,year . ,_)
	       (decode-time time)))
    (format "%02d-%s-%04d"
	    day
	    (aref ["Jan" "Feb" "Mar" "Apr" "May" "Jun"
		   "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]
		  (1- month))
	    year)))

(defun bic--print-sexp (sexp)
  "Print SEXP in current buffer.
Like `prin1', but escape newlines, and bind variables to avoid surprises."
  (let ((print-escape-newlines t)
	(print-level nil)
	(print-length nil)
	(print-circle nil))
    (prin1 sexp (current-buffer))))

(defun bic--write-string-to-file (string file)
  "Write STRING to FILE, overwriting any previous contents."
  (with-temp-buffer
    (insert string)
    (write-region (point-min) (point-max) file nil :silent)))

(defun bic-list-status (account)
  (interactive (list (bic--read-running-account)))
  (when (stringp account)
    (setq account (bic--find-account account)))
  (fsm-send account '(:queue-task (:any-mailbox :list-status-all))))

(defun bic-list-status-all ()
  (interactive)
  (mapc #'bic-list-status bic-running-accounts))

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

;;; Utility functions

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
  ;; Present the "real" name for completion etc, and return the UTF-7
  ;; encoded name that's used in IMAP commands.
  (let* ((mailboxes (bic--directory-directories (expand-file-name account bic-data-directory) "[^.]"))
	 (decoded-and-utf7
	  (mapcar
	   (lambda (mailbox-dir-name)
	     (let ((utf7-name (bic--unsanitize-mailbox-name mailbox-dir-name)))
	       (cons (utf7-decode utf7-name t) utf7-name)))
	   mailboxes))
	 (selected
	  (completing-read prompt decoded-and-utf7 nil require-match)))
    (cdr (assoc selected decoded-and-utf7))))

(provide 'bic)
;;; bic.el ends here
