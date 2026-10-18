;;; bic-smtpmail.el --- toggle smtpmail offline status based on BIC  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Magnus Henoch

;; Author: Magnus Henoch <magnus.henoch@gmail.com>

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

;; `smtpmail' has a feature to queue messages that the user attempts
;; to send, and to send queued messages later.  However, you have to
;; set `smtpmail-queue-mail' and run `smtpmail-send-queued-mail'
;; manually.  This file does that for you, under the assumption that
;; when BIC has a connection to an IMAP server, you should be able to
;; send messages through SMTP, and vice versa.
;;
;; To use this feature, set `bic-smtpmail-toggle-queueing' to t.

;;; Code:

(require 'bic)
(require 'smtpmail)
(require 'auth-source)
(require 'bic-sasl-xoauth2)
(require 'url-parse)
(require 'url-util)
(require 'mail-utils)
(require 'subr-x)

(defvar default-file-modes)
(declare-function bic-sasl-xoauth2-access-token "bic-sasl-xoauth2"
                  (host user))

(defcustom bic-smtpmail-sender-account-alist nil
  "Map alternate message sender addresses to BIC account addresses.
Each entry is (SENDER . ACCOUNT), with case-insensitive addresses."
  :group 'bic
  :type '(repeat (cons (string :tag "Message sender")
                       (string :tag "BIC account"))))

(defcustom bic-smtpmail-sender-settings-alist nil
  "Map message sender addresses to explicit SMTP (HOST PORT STREAM-TYPE).
STREAM-TYPE should be `ssl' or `starttls'.  These settings take
precedence over discovered or cached SMTP settings."
  :group 'bic
  :type '(repeat (list (string :tag "Message sender")
                       (string :tag "SMTP host")
                       (integer :tag "SMTP port")
                       (choice (const starttls) (const ssl)))))

(defvar bic-smtpmail--account-context nil
  "Dynamically bound account data for the current BIC SMTP send.")

(defvar bic-smtpmail--allow-prompt nil
  "Non-nil when first-use SMTP discovery may prompt the user.")

(defun bic-smtpmail--address (header)
  "Extract a single normalized email address from HEADER, or nil."
  (when header
    (let ((address (cadr (mail-extract-address-components header))))
      (and (stringp address) (downcase address)))))

(defun bic-smtpmail--account-for-address (address)
  "Return saved BIC account data matching ADDRESS, if any."
  (when (stringp address)
    (let* ((account-address
            (or (cdr (cl-find-if
                      (lambda (entry)
                        (string-equal-ignore-case (car entry) address))
                      bic-smtpmail-sender-account-alist))
                address))
           (saved-name
            (cl-find-if (lambda (name)
                          (string-equal-ignore-case name account-address))
                        (and (file-directory-p bic-data-directory)
                             (directory-files bic-data-directory nil "@"))))
           (account-dir (and saved-name
                             (expand-file-name saved-name bic-data-directory)))
           (file (and account-dir
                      (expand-file-name "connection-info" account-dir))))
      (when (and file (file-readable-p file))
        (let* ((url (with-temp-buffer
                      (insert-file-contents-literally file)
                      (string-trim (buffer-string))))
               (parsed (url-generic-parse-url url)))
          (list :address saved-name :sender address :dir account-dir
                :imap-host (url-host parsed)
                :imap-user (url-unhex-string (url-user parsed))
                :imap-port (or (url-port parsed)
                               (if (equal (url-type parsed) "imaps") 993 143))))))))

(defun bic-smtpmail--read-smtp-settings (account)
  "Read a previously saved SMTP URL for ACCOUNT."
  (let ((file (expand-file-name "smtp-connection-info"
                                (plist-get account :dir))))
    (when (file-readable-p file)
      (let* ((url (with-temp-buffer
                    (insert-file-contents-literally file)
                    (string-trim (buffer-string))))
             (parsed (url-generic-parse-url url)))
        (unless (and (member (url-type parsed) '("smtp" "smtps"))
                     (url-host parsed))
          (error "Invalid SMTP connection info in %s" file))
        (list (url-host parsed)
              (or (url-port parsed)
                  (if (equal (url-type parsed) "smtps") 465 587))
              (if (equal (url-type parsed) "smtps") 'ssl 'starttls))))))

(defun bic-smtpmail--lookup-srv-settings (domain service stream-type)
  "Look up SMTP SERVICE for DOMAIN and return (HOST PORT STREAM-TYPE).
Report lookup errors and return nil when no usable SRV target exists."
  (let ((target (format "_%s._tcp.%s" service domain)))
    (condition-case err
        (cl-loop for candidate in (srv-lookup target)
                 for host = (car-safe candidate)
                 for port = (cdr-safe candidate)
                 when (and (stringp host)
                           (not (member host '("" ".")))
                           (integerp port)
                           (<= 1 port 65535))
                 return (list host port stream-type))
      (error
       (message "BIC SMTP: SRV lookup for %s failed: %s"
                target (error-message-string err))
       nil))))

(defun bic-smtpmail--discover-settings (address)
  "Discover secure SMTP settings for ADDRESS, prompting if needed."
  (let* ((domain (downcase (cadr (split-string address "@"))))
         (settings (or (bic-smtpmail--lookup-srv-settings
                        domain "submissions" 'ssl)
                       (bic-smtpmail--lookup-srv-settings
                        domain "submission" 'starttls))))
    (cond
     (settings settings)
     ((not bic-smtpmail--allow-prompt)
      (user-error "No SMTP server discovered for %s; configure smtpmail-smtp-server or send interactively" address))
     (t
      (let* ((response (read-string
                        (format "SMTP server for %s (hostname or hostname:port): "
                                address)))
             (colon (cl-position ?: response))
             (host (if colon (substring response 0 colon) response))
             (port (if colon
                       (string-to-number (substring response (1+ colon)))
                     587)))
        (unless (and (not (string-empty-p host))
                     (<= 1 port 65535))
          (user-error "Invalid SMTP host or port"))
        (list host port (if (= port 465) 'ssl 'starttls)))))))

(defun bic-smtpmail--save-settings (account settings)
  "Save successful SMTP SETTINGS for ACCOUNT with private file modes."
  (pcase-let ((`(,host ,port ,stream) settings))
    (let ((file (expand-file-name "smtp-connection-info"
                                  (plist-get account :dir)))
          (default-file-modes #o600))
      (with-temp-file file
        (insert (format "%s://%s:%d\n"
                        (if (eq stream 'ssl) "smtps" "smtp") host port)))
      (set-file-modes file #o600))))

(defun bic-smtpmail--call-with-account-settings (function &rest args)
  "Call FUNCTION with BIC-discovered endpoint settings when applicable."
  (let* ((sender (save-restriction
                   (when (fboundp 'message-narrow-to-headers)
                     (message-narrow-to-headers))
                   (or (and mail-specify-envelope-from
                            (bic-smtpmail--address (mail-envelope-from)))
                       (bic-smtpmail--address (mail-fetch-field "from")))))
         (account (bic-smtpmail--account-for-address sender))
         (sender-settings
          (cdr (cl-find-if
                (lambda (entry)
                  (string-equal-ignore-case (car entry) sender))
                bic-smtpmail-sender-settings-alist)))
         (cached (and account (bic-smtpmail--read-smtp-settings account)))
         ;; Any explicitly configured smtpmail server is authoritative.
         (explicit (and smtpmail-smtp-server
                        (not (equal smtpmail-smtp-server ""))))
         (settings (cond (explicit
                          (list smtpmail-smtp-server smtpmail-smtp-service
                                smtpmail-stream-type))
                         (sender-settings sender-settings)
                         (cached cached)
                         (account
                          (bic-smtpmail--discover-settings
                           (plist-get account :address)))))
         (result nil))
    (let ((bic-smtpmail--account-context account)
          (smtpmail-smtp-server (or (car-safe settings) smtpmail-smtp-server))
          (smtpmail-smtp-service (or (nth 1 settings) smtpmail-smtp-service))
          (smtpmail-stream-type (or (nth 2 settings) smtpmail-stream-type)))
      (setq result
            (if account
                (apply #'bic-smtpmail--call-with-auth function args)
              (apply function args))))
    ;; Save the discovered endpoint only after smtpmail returned successfully.
    (when (and account settings (not explicit) (not cached))
      (bic-smtpmail--save-settings account settings))
    result))

(defcustom bic-smtpmail-auto-xoauth2 t
  "If non-nil, prefer BIC XOAUTH2 when an SMTP server offers it.
An explicit `smtp-auth' setting in auth-source takes precedence.  A
customized `smtpmail-auth-supported' also takes precedence."
  :group 'bic
  :type 'boolean)

(defcustom bic-smtpmail-toggle-queueing nil
  "If non-nil, toggle `smtpmail-queue-mail' based on BIC status.
If BIC is connected to at least one server that isn't localhost,
turn queueing off and send any queued messages.
Otherwise, turn queueing on, such that messages \"sent\" will
be queued, in order to be sent when you're online again."
  :group 'bic
  :type 'boolean)

(defun bic-smtpmail--auth-method-list (value)
  "Convert auth-source SMTP auth VALUE to a list of method symbols."
  (mapcar (lambda (method)
	    (if (symbolp method)
		method
	      (intern-soft (downcase method))))
	  (ensure-list value)))

(defun bic-smtpmail--auth-source-search-with-token
    (original host port user &rest args)
  "Temporarily wrap `auth-source-search' while sending via SMTP.
ORIGINAL is the function being wrapped.  For matching SMTP HOST, PORT,
and USER searches, allow entries without a secret and add a placeholder
secret so smtpmail will try XOAUTH2.  The SMTP authentication wrapper
supplies the actual access token; all other searches go to ORIGINAL."
  (let* ((query-host (plist-get args :host))
	 (query-port (plist-get args :port))
	 (query-user (plist-get args :user)))
    (if (not (and (equal query-host host)
		  (equal (and query-port (format "%s" query-port)) port)
		  (or (null query-user) (equal query-user user))))
	(apply original args)
      ;; smtpmail requires a secret before it attempts authentication.
      ;; Remove that requirement so OAuth-only entries can contribute a
      ;; username; the XOAUTH2 method wrapper supplies the real token.
      (let* ((account bic-smtpmail--account-context)
	     (lookup-args (copy-sequence args))
	     (requirements (ensure-list (plist-get lookup-args :require)))
	     (lookup-args (plist-put lookup-args :require
				     (delq :secret requirements)))
	     (smtp-results (apply original lookup-args))
	     (imap-args (and account (copy-sequence lookup-args))))
	(when imap-args
	  (setq imap-args
		(plist-put imap-args :host (plist-get account :imap-host)))
	  (setq imap-args
		(plist-put imap-args :port
			   (list (if (= (plist-get account :imap-port) 993)
				     "imaps" "imap")
				 (plist-get account :imap-port))))
	  (unless (plist-get imap-args :user)
	    (setq imap-args
		  (plist-put imap-args :user
			     (plist-get account :imap-user)))))
	(let* ((results (or smtp-results
			    (and imap-args (apply original imap-args))))
	       (auth-info (or (car results)
			      (list :host host :port port :user user))))
	  (unless (plist-get auth-info :user)
	    (setq auth-info (plist-put auth-info :user user)))
	  (unless (plist-get auth-info :secret)
	    (setq auth-info (plist-put auth-info :secret
				       "bic-smtpmail-xoauth2")))
	  (cons auth-info (cdr results)))))))

(defvar bic-smtpmail--use-bic-auth nil
  "Non-nil while a BIC SMTP wrapper is sending mail.")

(defun bic-smtpmail--auth-source-search-with-imap-fallback
    (original host port account &rest args)
  "Search SMTP auth-source ARGS, falling back to ACCOUNT's IMAP entry."
  (let ((results (apply original args)))
    (if (or results
            (not account)
            (not (and (equal (plist-get args :host) host)
                      (equal (format "%s" (plist-get args :port)) port))))
        results
      (let ((imap-args (copy-sequence args)))
        (setq imap-args
              (plist-put imap-args :host (plist-get account :imap-host)))
        (setq imap-args
              (plist-put imap-args :port
                         (list (if (= (plist-get account :imap-port) 993)
                                   "imaps" "imap")
                               (plist-get account :imap-port))))
        (unless (plist-get imap-args :user)
          (setq imap-args
                (plist-put imap-args :user (plist-get account :imap-user))))
        (apply original imap-args)))))

(defun bic-smtpmail--try-auth-methods (original process supported-extensions
                                      host port &optional ask-for-password)
  "Use BIC XOAUTH2 when available, otherwise delegate to ORIGINAL."
  (if (not bic-smtpmail--use-bic-auth)
      (funcall original process supported-extensions host port ask-for-password)
    (let* ((port (if port (format "%s" port) "smtp"))
           (account bic-smtpmail--account-context)
           (auth-info
            (or (car (auth-source-search :host host :port port
                                         :user smtpmail-smtp-user :max 1))
                (and account
                     (car (auth-source-search
                           :host (plist-get account :imap-host)
                           :port (list (if (= (plist-get account :imap-port) 993)
                                           "imaps" "imap")
                                       (plist-get account :imap-port))
                           :user (or smtpmail-smtp-user
                                     (plist-get account :imap-user))
                           :max 1)))))
           (user (or (plist-get auth-info :user)
                     smtpmail-smtp-user
                     (and account (plist-get account :imap-user))
                     smtpmail-mail-address))
           (explicit-methods (bic-smtpmail--auth-method-list
                              (plist-get auth-info :smtp-auth)))
           (xoauth2-advertised
            (memq 'xoauth2 (cdr-safe (assq 'auth supported-extensions))))
           (auto-selection
            (and bic-smtpmail-auto-xoauth2
                 (null explicit-methods)
                 (or (memq 'xoauth2 smtpmail-auth-supported)
                     (equal smtpmail-auth-supported '(cram-md5 plain login)))))
           (explicit-xoauth2 (memq 'xoauth2 explicit-methods))
           (oauth-host (or (plist-get bic-smtpmail--account-context :imap-host)
                           host))
           (oauth-info (and user (bic-sasl-xoauth2-resolve-urls oauth-host user)))
           (use-bic-xoauth2
            (and xoauth2-advertised
                 (nth 3 oauth-info)
                 (or auto-selection explicit-xoauth2))))
      (if (not use-bic-xoauth2)
          (let ((auth-source-search-original
                 (symbol-function 'auth-source-search)))
            (cl-letf (((symbol-function 'auth-source-search)
                       (lambda (&rest args)
                         (apply #'bic-smtpmail--auth-source-search-with-imap-fallback
                                auth-source-search-original host port account args))))
              (funcall original process supported-extensions host port
                       ask-for-password)))
        (let* ((password (auth-info-password auth-info))
               (oauth-token (bic-sasl-xoauth2-access-token oauth-host user))
               (smtpmail-auth-supported
                (if explicit-methods
                    smtpmail-auth-supported
                  (cons 'xoauth2
                        (if password
                            (delq 'xoauth2
                                  (copy-sequence smtpmail-auth-supported))
                          (list)))))
               (auth-source-search-original
                (symbol-function 'auth-source-search))
               (smtpmail-try-auth-method-original
                (symbol-function 'smtpmail-try-auth-method)))
          (cl-letf (((symbol-function 'auth-source-search)
                     (lambda (&rest args)
                       (apply #'bic-smtpmail--auth-source-search-with-token
                              auth-source-search-original host port user args)))
                    ((symbol-function 'smtpmail-try-auth-method)
                     (lambda (smtp-process mechanism smtp-user password)
                       (funcall smtpmail-try-auth-method-original
                                smtp-process mechanism smtp-user
                                (if (eq mechanism 'xoauth2)
                                    oauth-token password)))))
            (funcall original process supported-extensions host port
                     ask-for-password)))))))

(defun bic-smtpmail--call-with-auth (function &rest args)
  "Call FUNCTION with ARGS, enabling BIC SMTP authentication locally."
  (let ((bic-smtpmail--use-bic-auth t)
            (smtpmail-try-auth-methods-original
         (symbol-function 'smtpmail-try-auth-methods)))
    (cl-letf (((symbol-function 'smtpmail-try-auth-methods)
               (lambda (&rest auth-args)
                 (apply #'bic-smtpmail--try-auth-methods
                        smtpmail-try-auth-methods-original auth-args))))
      (apply function args))))

;;;###autoload
(defun bic-smtpmail-send-it (&optional interactivep)
  "Send the current message with `smtpmail-send-it' and BIC auth support.
Use this as the value of `send-mail-function' or
`message-send-mail-function'.  XOAUTH2 is enabled only for this send
operation; other callers of `smtpmail' retain its normal behavior."
  (interactive (list t))
  (let ((bic-smtpmail--allow-prompt interactivep))
    (bic-smtpmail--call-with-account-settings #'smtpmail-send-it)))

;;;###autoload
(defun bic-smtpmail-send-queued-mail ()
  "Send queued messages with BIC SMTP authentication enabled."
  (interactive)
  (let ((bic-smtpmail--allow-prompt nil)
        (smtpmail-via-smtp-original (symbol-function 'smtpmail-via-smtp)))
    (cl-letf (((symbol-function 'smtpmail-via-smtp)
               (lambda (&rest args)
                 (apply #'bic-smtpmail--call-with-account-settings
                        smtpmail-via-smtp-original args))))
      (smtpmail-send-queued-mail))))

(defun bic-smtpmail--online-p ()
  "Are we online enough that we should try to send mail?
Check whether at least one account whose server is not
\"localhost\" is online."
  (let ((onlinep nil))
    (maphash
     (lambda (account state)
       (when (eq state :connected)
	 (unless (string= "localhost"
			  (plist-get
			   (fsm-get-state-data (bic--find-account account))
			   :server))
	   (setq onlinep t))))
     bic-account-state-table)
    onlinep))

;;;###autoload
(defun bic-smtpmail--state-update (&optional _account _new-state)
  "Update smtpmail state when going online/offline.
If going offline, start queuing outgoing messages.
If going online, send queued messages.

This function is meant to be added to
`bic-account-state-update-functions'."
  (when bic-smtpmail-toggle-queueing
    (pcase (cons smtpmail-queue-mail (bic-smtpmail--online-p))
      (`(nil . nil)
       ;; Went offline
       (setq smtpmail-queue-mail t)
       :went-offline)
      (`(t . t)
       ;; Went online
       (setq smtpmail-queue-mail nil)
       ;; XXX: what if sending fails?  If so, need to reset `smtpmail-queue-mail'...
       ;; Perhaps display errors as warnings, to get attention?
       (run-with-idle-timer 10 nil 'bic-smtpmail-send-queued-mail))
      (`(nil . t)
       :still-online)
      (`(t . nil)
       :still-offline)
      (other
       (warn "Unknown state %S" other)))))

;;;###autoload
(with-eval-after-load "bic"
  (add-hook 'bic-account-state-update-functions 'bic-smtpmail--state-update))

(provide 'bic-smtpmail)
;;; bic-smtpmail.el ends here
