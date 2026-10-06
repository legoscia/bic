;;; bic-sasl.el --- helper functions for SASL authentication  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Magnus Henoch

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

;; 

;;; Code:

(require 'sasl)
(require 'oauth2)
(require 'bic-sasl-xoauth2)

(defun bic--sasl-find-mechanism (server-mechanisms username server)
  "Find a suitable SASL authentication mechanism.
Like `sasl-find-mechanism', but prefer XOAUTH2 if we can get it.

SERVER-MECHANISMS is a list of strings, the authentication
mechanisms supported by the server.
USERNAME is the user name we would authenticate with.
SERVER is the server we're authenticating to."
  ;; If we have a "client id" for the given username and server,
  ;; prefer authenticating with XOAUTH2 if the server supports it.
  ;;
  ;; If we don't have a client id, trying to use XOAUTH2 would
  ;; result in the user being asked for a client id, which
  ;; would not be a particularly good user experience.
  (if (nth 3 (bic-sasl-xoauth2-resolve-urls server username))
      (let ((sasl-mechanisms (cons "XOAUTH2" sasl-mechanisms))
	    (sasl-mechanism-alist (cons (list "XOAUTH2" 'bic-sasl-xoauth2) sasl-mechanism-alist)))
	(sasl-find-mechanism server-mechanisms))
    (sasl-find-mechanism server-mechanisms)))

(defun bic--sasl-next-step (client step)
  "Like `sasl-next-step', but with some BIC specific quirks.

CLIENT is the value returned by `sasl-make-client', and STEP is
either nil (for the first call) or the value returned by this
function (for subsequent calls)."
  (cl-letf*
      (
;;; If getting an OAuth token through an HTTP request fails, the
;;; server is likely to return a 401 Unauthorized response.  This
;;; makes the url library ask the user for a username and password in
;;; order to retry the request with HTTP Basic authentication.  That
;;; makes the request fail because the Authorization header thus
;;; generated conflicts with the data in the POST body - and there is
;;; no user-accessible way to clear the username and password
;;; provided.
;;;
;;; Let's avoid that by making `url-http-handle-authentication' return
;;; t immediately, meaning that the request has "succeeded" and the
;;; current response is to be returned.
;;;
;;; XXX: there is advice for this in oauth2.el.  Why doesn't that work?
       ((symbol-function 'url-http-handle-authentication)
	(lambda (_proxy)
	  t))
;;; If a request for an OAuth token fails, `oauth2-auth-and-store'
;;; will store the JSON error message as if it were an OAuth token.
;;; Of course, since it is not a token but an error message, any
;;; requests we try to authenticate with it will fail.  The oauth2
;;; library will notice that it is not a valid token, but since it
;;; thinks it's just expired, it will try to "refresh" it, which
;;; doesn't work because there is nothing to refresh.  Let's work
;;; around that by throwing an error before we have a chance to store
;;; the error message.
       (oauth2-make-access-request-orig (symbol-function 'oauth2-make-access-request))
       ((symbol-function 'oauth2-make-access-request)
	(lambda (url data)
	  (let* ((result (funcall oauth2-make-access-request-orig url data))
		 (error-code (assq 'error result))
		 (error-description (assq 'error_description result)))
	    (if error-code
		(throw :bic-sasl-abort
		       (cons :fail
			     (format "Error getting OAuth token: %s, %s"
				     (cdr error-code)
				     (cdr error-description))))
	      result)))))
;;; oauth2.el uses plstore.el to encrypt OAuth tokens when writing
;;; them to disk, asking the user for a passphrase.  However, BIC is
;;; supposed to run in the background without asking the user for
;;; anything, even if the connection is lost and then reestablished.
;;; 
    (sasl-next-step client step)))

(provide 'bic-sasl)
;;; bic-sasl.el ends here
