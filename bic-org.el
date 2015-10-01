;;; bic-org.el --- link to BIC messages from org-mode  -*- lexical-binding: t; -*-

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

;; 

;;; Code:

(require 'bic)

;;;###autoload
(defun bic-org-store-link ()
  (let ((full-uid
	 (cond
	  ((derived-mode-p 'bic-message-mode)
	   bic-message--full-uid)
	  ((derived-mode-p 'bic-mailbox-mode)
	   ;; In a mailbox buffer, create a link to the message under point.
	   (ewoc-data (ewoc-locate bic-mailbox--ewoc (point)))))))
    (when full-uid
      (let* ((link (concat "bic:"
			   (bic--sanitize-mailbox-name bic--current-account)
			   ":"
			   (bic--sanitize-mailbox-name bic--current-mailbox)
			   ":"
			   full-uid))
	     (mailbox-buffer (bic-mailbox--find-buffer
			      bic--current-account bic--current-mailbox))
	     (hashtable (when mailbox-buffer
			  (buffer-local-value 'bic-mailbox--hashtable mailbox-buffer)))
	     (envelope (when hashtable
			 (gethash full-uid hashtable))))
	(pcase envelope
	  (`(,date ,subject ,from ,_sender ,_reply-to ,to ,_cc ,_bcc ,_in-reply-to ,message-id)
	   (org-store-link-props
	    :type "bic"
	    :link link
	    :date date
	    :subject subject
	    :message-id message-id
	    :from (pcase from
		    (`((,name ,_source-route ,mailbox-name ,host-name) . ,_)
		     (mail-header-make-address name (concat mailbox-name "@" host-name))))
	    :to (pcase to
		  (`((,name ,_source-route ,mailbox-name ,host-name) . ,_)
		   (mail-header-make-address name (concat mailbox-name "@" host-name)))))
	   (org-add-link-props :description (org-email-link-description))
	   link))))))

;;;###autoload
(with-eval-after-load "org"
  (add-to-list 'org-store-link-functions 'bic-org-store-link))

;;;###autoload
(defun bic-org-follow (link)
  (unless (string-match "\\`\\([^:]+\\):\\([^:]+\\):\\([0-9-]+\\)\\'" link)
    (error "Invalid BIC link %S" link))
  (let ((account (match-string 1 link))
	(mailbox (match-string 2 link))
	(full-uid (match-string 3 link)))
    (setq account (bic--unsanitize-mailbox-name account))
    (setq mailbox (bic--unsanitize-mailbox-name mailbox))
    (bic-message-display account mailbox full-uid)))

;;;###autoload
(with-eval-after-load "org"
  (org-add-link-type "bic" 'bic-org-follow))

(provide 'bic-org)
;;; bic-org.el ends here
