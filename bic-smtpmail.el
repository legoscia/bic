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

(defcustom bic-smtpmail-toggle-queueing nil
  "If non-nil, toggle `smtpmail-queue-mail' based on BIC status.
If BIC is connected to at least one server that isn't localhost,
turn queueing off and send any queued messages.
Otherwise, turn queueing on, such that messages \"sent\" will
be queued, in order to be sent when you're online again."
  :group 'bic
  :type 'boolean)

(defun bic-smtpmail--online-p ()
  ;; Check whether at least one account whose server is not
  ;; "localhost" is online.
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
       (run-with-idle-timer 10 nil 'smtpmail-send-queued-mail))
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
