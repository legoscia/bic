;;; bic-sasl-xoauth2.el --- OAuth 2.0 SASL authentication for BIC

;; Copyright (C) 2018 Kazuhiro Ito
;; Copyright (C) 2020 Magnus Henoch

;; Author: Kazuhiro Ito <kzhr@d1.dion.ne.jp>
;; Maintainer: Magnus Henoch <magnus.henoch@gmail.com>
;; Keywords: SASL, OAuth 2.0
;; Created: January 2018

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is a SASL interface layer for OAuth 2.0 authorization message.
;; It was forked from sasl-xoauth2.el in FLIM; see
;; https://github.com/wanderlust/flim/tree/flim-1_14-wl/ for the
;; original.

;;; Requirements:
;;
;; * oauth2.el
;; https://elpa.gnu.org/packages/oauth2.html

;;; Usage
;;
;; 1. Set up bic-sasl-xoauth2-host-url-table and
;; bic-sasl-xoauth2-host-user-id-table variables.
;;
;; 2. When passphrase is asked, input client secret.

;;; Code:

(require 'sasl)
(require 'cl-lib)
(require 'oauth2)
(require 'url-parse)

(defconst bic-sasl-xoauth2-steps
  '(bic-sasl-xoauth2-response))

(defgroup bic-sasl-xoauth2 nil
  "SASL interface layer for OAuth 2.0 authorization message."
  :group 'mail)

(defcustom bic-sasl-xoauth2-token-directory
  (expand-file-name "bic-sasl-xoauth2" user-emacs-directory)
  "Deprecated; BIC now keeps OAuth tokens in memory for this Emacs session."
  :type 'directory
  :group 'bic-sasl-xoauth2)

(defcustom bic-sasl-xoauth2-refresh-token-threshold 60
  "Refresh token if expiration limit is left less than specified seconds."
  :type 'number
  :group 'bic-sasl-xoauth2)

(defcustom bic-sasl-xoauth2-host-url-table
  '(;; Gmail
    ("gmail\\.com$"
     "https://accounts.google.com/o/oauth2/v2/auth"
     "https://www.googleapis.com/oauth2/v4/token"
     "https://mail.google.com/"
     nil)
    ;; Outlook.com
    ("outlook\\.com$"
     "https://login.live.com/oauth20_authorize.srf"
     "https://login.live.com/oauth20_token.srf"
     "wl.offline_access wl.imap"
     ;; You need register redirect URL at Application Registration Portal
     ;; https://apps.dev.microsoft.com/
     "http://localhost/result"))
  "List of OAuth 2.0 URLs.  Each element of list is regexp for host, auth-url, token-url, scope and redirect-uri (optional)."
      :type '(repeat (list
		      (regexp :tag "Regexp for Host")
		      (string :tag "Auth-URL")
		      (string :tag "Token-URL")
		      (string :tag "Scope")
		      (choice string (const :tag "none" nil))))
      :group 'bic-sasl-xoauth2)

(defcustom bic-sasl-xoauth2-host-user-id-table
  nil
  "List of OAuth 2.0 Client IDs.  Each element of list is regexp for host, regexp for User ID, client ID and client secret (optional).
Below is example to use Thunderbird's client ID and secret (not recommended, just an expample).

(setq bic-sasl-xoauth2-host-user-id-table
      (list (list \"\\\\.gmail\\\\.com$\"
	          \".\"
	          \"91623021742-ud877vhta8ch9llegih22bc7er6589ar.apps.googleusercontent.com\"
	          \"iBn5rLbhbm_qoPbdGkgX81Dj\")))
"
  :type '(repeat (list
		  (regexp :tag "Regexp for Host")
		  (regexp :tag "Regexp for User ID")
		  (string :tag "Client ID")
		  (choice :tag "Client Secret"
			  string
			  (const :tag "none" nil))))
  :group 'bic-sasl-xoauth2)


;; This advice makes oauth2.el to keep the time of getting token.
(defadvice oauth2-make-access-request (after bic-sasl-xoauth2 disable)
  (setq ad-return-value (cons `(auth_time . ,(current-time))
			      ad-return-value)))

;; Modified version of oauth2-refresh-access.  It keeps refreshed time
;; and updates expires_in parameter.
(defun bic-sasl-xoauth2-refresh-access (token)
  "Refresh OAuth access TOKEN.
TOKEN should be obtained with `oauth2-request-access'."
  (let ((response
	 (oauth2-make-access-request
          (oauth2-token-token-url token)
          (concat "client_id=" (oauth2-token-client-id token)
                  "&client_secret=" (oauth2-token-client-secret token)
                  "&refresh_token=" (oauth2-token-refresh-token token)
                  "&grant_type=refresh_token"))))
    (setf (oauth2-token-access-token token)
          (cdr (assq 'access_token response)))
    ;; Update authorization time.
    (setcdr (assq 'auth_time (oauth2-token-access-response token))
	    (current-time))
    ;; Update expires_in parameter.
    (cond
     ((and (assq 'expires_in (oauth2-token-access-response token))
	   (assq 'expires_in response))
      (setcdr (assq 'expires_in (oauth2-token-access-response token))
	      (cdr (assq 'expires_in response))))
     ((assq 'expires_in (oauth2-token-access-response token))
      (let ((list (memq (assq 'expires_in (oauth2-token-access-response token))
			(oauth2-token-access-response token))))
	(setcdr list (cdr list))))
     ((assq 'expires_in response)
      (setf (oauth2-token-access-response token)
	    (cons (assq 'expires_in response)
		  (oauth2-token-access-response token))))))
  ;; If the token has a plstore, update it
  (let ((plstore (oauth2-token-plstore token)))
    (when plstore
      (plstore-put plstore (oauth2-token-plstore-id token)
                   nil `(:access-token
                         ,(oauth2-token-access-token token)
                         :refresh-token
                         ,(oauth2-token-refresh-token token)
                         :access-response
                         ,(oauth2-token-access-response token)
                         ))
      (plstore-save plstore)))
  token)

(defun bic-sasl-xoauth2-resolve-urls (host user)
  (let (auth-url token-url client-id scope redirect-uri client-secret)
    (let ((table bic-sasl-xoauth2-host-url-table))
      (while table
	(when (string-match (caar table) host)
	  (setq auth-url  (nth 1 (car table))
		token-url (nth 2 (car table))
		scope     (nth 3 (car table))
		redirect-uri (nth 4 (car table))
		table nil))
	(setq table (cdr table))))
    (let ((table bic-sasl-xoauth2-host-user-id-table))
      (while table
	(when (and (string-match (caar table) host)
		   (string-match (nth 1 (car table)) user))
	  (setq client-id (nth 2 (car table))
		client-secret (nth 3 (car table))
		table nil))
	(setq table (cdr table))))
    (list auth-url token-url scope client-id client-secret redirect-uri)))

(defun bic-sasl-xoauth2-token-expired-p (token)
  (let ((access-response (oauth2-token-access-response token)))
    (or (null (assq 'expires_in access-response))
	(time-less-p
	 (time-add (cdr (assq 'auth_time access-response))
		   (cdr (assq 'expires_in access-response)))
	 (time-add (current-time)
	   (- bic-sasl-xoauth2-refresh-token-threshold))))))

(defun bic-sasl-xoauth2--callback-filter (process data)
  "Handle the loopback HTTP callback received by PROCESS with DATA."
  (let ((request (concat (or (process-get process 'bic-request) "") data)))
    (process-put process 'bic-request request)
    (when (string-match "\\`GET \\([^ ]+\\) HTTP/1\\.[01]" request)
      (let* ((target (match-string 1 request))
             (query (and (string-match "\\?\\(.*\\)" target)
                         (url-parse-query-string (match-string 1 target))))
             (code (cadr (assoc "code" query)))
             (state (cadr (assoc "state" query)))
             (error (cadr (assoc "error" query))))
        (cond
         ((not (equal state bic-sasl-xoauth2--callback-state))
          (message "BIC OAuth callback rejected: state was missing or did not match")
          (process-send-string
           process
           "HTTP/1.1 400 Bad Request\r\nConnection: close\r\nContent-Length: 0\r\n\r\n"))
         (t
          (setq bic-sasl-xoauth2--callback-result
                (cond (error (cons :error error))
                      (code code)
                      (t (cons :error "No authorization code in callback"))))
          (process-send-string
           process
           (concat "HTTP/1.1 200 OK\r\nContent-Type: text/plain; charset=utf-8\r\n"
                   "Connection: close\r\n\r\n"
                   "Authorization received. You can close this browser tab."))))
        (delete-process process)))))

(defvar bic-sasl-xoauth2--callback-state nil)
(defvar bic-sasl-xoauth2--callback-result nil)
(defvar bic-sasl-xoauth2--token-cache (make-hash-table :test 'equal)
  "OAuth tokens held in memory for the current Emacs session.")

(defun bic-sasl-xoauth2--token-file (user)
  "Return BIC's persistent OAuth token file for USER."
  (expand-file-name
   "oauth-token"
   (expand-file-name user
                     (if (boundp 'bic-data-directory)
                         bic-data-directory
                       (locate-user-emacs-file "bic")))))

(defun bic-sasl-xoauth2--read-token (file client-id client-secret auth-url token-url)
  "Read a cached OAuth token from FILE, or return nil if absent."
  (when (file-exists-p file)
    (condition-case err
        (let* ((read-eval nil)
               (data (with-temp-buffer
                       (insert-file-contents-literally file)
                       (read (current-buffer)))))
          (when (and (stringp (plist-get data :access-token))
                     (stringp (plist-get data :refresh-token))
                     (listp (plist-get data :access-response)))
            (make-oauth2-token
             :client-id client-id :client-secret client-secret
             :access-token (plist-get data :access-token)
             :refresh-token (plist-get data :refresh-token)
             :access-response (plist-get data :access-response)
             :auth-url auth-url :token-url token-url)))
      (error
       (message "Ignoring unreadable BIC OAuth token file %s: %s"
                file (error-message-string err))
       nil))))

(defun bic-sasl-xoauth2--write-token (token file)
  "Write TOKEN to FILE with owner-only permissions."
  (let ((directory (file-name-directory file)))
    (with-file-modes #o700
      (make-directory directory t))
    (with-temp-buffer
      (prin1 `(:access-token ,(oauth2-token-access-token token)
                             :refresh-token ,(oauth2-token-refresh-token token)
                             :access-response ,(oauth2-token-access-response token))
             (current-buffer))
      (insert "\n")
      (let ((default-file-modes #o600))
        (write-region (point-min) (point-max) file nil :silent)))
    (set-file-modes file #o600)))

(defun bic-sasl-xoauth2--wait-for-callback ()
  "Wait for the loopback callback and return its authorization code."
  (while (not bic-sasl-xoauth2--callback-result)
    (accept-process-output nil 0.1))
  (if (and (consp bic-sasl-xoauth2--callback-result)
           (eq (car bic-sasl-xoauth2--callback-result) :error))
      (error "OAuth authorization failed: %s"
             (cdr bic-sasl-xoauth2--callback-result))
    bic-sasl-xoauth2--callback-result))

(defun bic-sasl-xoauth2--auth-and-store
    (auth-url token-url scope client-id client-secret redirect-uri user)
  "Return a cached OAuth token or acquire one without plstore."
  (let* ((cache-key (list :persistent-token-v1
                         auth-url token-url scope client-id user))
         (file (bic-sasl-xoauth2--token-file user))
         (cached-token (or (gethash cache-key bic-sasl-xoauth2--token-cache)
                           (bic-sasl-xoauth2--read-token
                            file client-id client-secret auth-url token-url))))
    (when cached-token
      (puthash cache-key cached-token bic-sasl-xoauth2--token-cache))
    (or cached-token
        (let ((token
               (if redirect-uri
                   (oauth2-auth auth-url token-url client-id client-secret
                                scope nil redirect-uri user)
                 (let* ((bic-sasl-xoauth2--callback-state
                         (secure-hash 'sha256
                                      (format "%s%s%s" (random) (current-time)
                                              (garbage-collect))))
                        (bic-sasl-xoauth2--callback-result nil)
                        (server (make-network-process
                                 :name "bic-oauth-callback" :server t
                                 :host "127.0.0.1" :service 0 :family 'ipv4
                                 :noquery t
                                 :filter #'bic-sasl-xoauth2--callback-filter))
                        (redirect-uri
                         (format "http://127.0.0.1:%d/"
                                 (aref (process-contact server :local) 4))))
                   (unwind-protect
                       (let ((oauth2-request-authorization-original
                              (symbol-function 'oauth2-request-authorization)))
                         (cl-letf (((symbol-function 'oauth2-request-authorization)
                                    (lambda (&rest request-args)
                                      (let ((read-string-original
                                             (symbol-function 'read-string)))
                                        (setf (nth 4 request-args) redirect-uri)
                                        (setf (nth 3 request-args)
                                              bic-sasl-xoauth2--callback-state)
                                        (cl-letf (((symbol-function 'read-string)
                                                   (lambda (prompt &rest args)
                                                     (if (string-prefix-p
                                                          "Follow the instruction on your default browser"
                                                          prompt)
                                                         (bic-sasl-xoauth2--wait-for-callback)
                                                       (apply read-string-original
                                                              prompt args)))))
                                          (apply oauth2-request-authorization-original
                                                 request-args))))))
                           (oauth2-auth auth-url token-url client-id client-secret
                                        scope nil redirect-uri user)))
                     (when (process-live-p server)
                       (delete-process server)))))))
          (puthash cache-key token bic-sasl-xoauth2--token-cache)
          (bic-sasl-xoauth2--write-token token file)
          token))))

(defun bic-sasl-xoauth2-response (client step &optional retry)
  (let ((host (sasl-client-server client))
	(user (sasl-client-name client))
	info access-token oauth2-token
	auth-url token-url client-id scope redirect-uri client-secret)
    (setq info (bic-sasl-xoauth2-resolve-urls host user)
	  auth-url
	  (or (car info)
	      (read-string (format "Input OAuth 2.0 AUTH-URL for %s: " host)))
	  token-url
	  (or (nth 1 info)
	      (read-string (format "Input OAuth 2.0 TOKEN-URL for %s: " host)))
	  scope
	  (or (nth 2 info)
	      (read-string (format "Input OAuth 2.0 SCOPE for %s: " host)))
	  client-id
	  (or (nth 3 info)
	      (read-string
	       (format "Input OAuth 2.0 CLIENT-ID for %s@%s: " user host)
	       user nil user))
	  client-secret
	  (or (nth 4 info)
	      (sasl-read-passphrase
	       (format "Input Oauth 2.0 CLIENT-SECRET for %s@%s: " user host)))
	  redirect-uri
	  (or (nth 5 info)
	      ;; Do not ask when bic-sasl-xoauth2-host-url-table is
	      ;; matched.
	      (unless (car info)
		(read-string
		 (format "Input OAuth 2.0 Redirect-URI for %s: " host)))))
    (setq oauth2-token
	  (progn
	    (ad-enable-advice 'oauth2-make-access-request 'after 'bic-sasl-xoauth2)
	    (ad-activate 'oauth2-make-access-request)
	    (prog1
		(bic-sasl-xoauth2--auth-and-store
		 auth-url token-url scope client-id client-secret redirect-uri user)
	      (ad-disable-advice 'oauth2-make-access-request
				 'after 'bic-sasl-xoauth2)
	      (ad-activate 'oauth2-make-access-request))))
    (when (bic-sasl-xoauth2-token-expired-p oauth2-token)
      (setq oauth-token (bic-sasl-xoauth2-refresh-access oauth2-token))
      (bic-sasl-xoauth2--write-token
       oauth2-token (bic-sasl-xoauth2--token-file user)))
    (setq access-token (oauth2-token-access-token oauth2-token))
    (format "user=%s\001auth=Bearer %s\001\001"
	    (sasl-client-name client)
	    access-token)))

(put 'bic-sasl-xoauth2 'sasl-mechanism
     (sasl-make-mechanism "XOAUTH2" bic-sasl-xoauth2-steps))

(provide 'bic-sasl-xoauth2)

;;; bic-sasl-xoauth2.el ends here
