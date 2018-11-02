;;; bic-message.el --- display a message             -*- lexical-binding: t; -*-

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
    (define-key map "n" 'bic-mailbox-next-unread)
    (define-key map " " 'bic-mailbox-next-page-or-next-unread)
    (define-key map "g" 'bic-message-reload)
    (define-key map "=" 'bic-message-identify)
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

;;;###autoload
(cl-defun bic-message-display (account mailbox msg &key raw)
  "Display a message in the buffer *BIC-Message*.
ACCOUNT and MAILBOX identify the mailbox that the message is in,
and MSG is a string, containing the uidvalidity of the mailbox
and the uid of the message, separated by a hyphen."
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
      (unless raw
	;; Gnus already does a fine job displaying messages, so we might
	;; as well piggy-back on that:
	(run-hooks 'gnus-article-decode-hook)
	(gnus-article-prepare-display)))
    (let ((window (display-buffer (current-buffer))))
      (set-window-start window (point-min)))))

(defun bic-message-reload (&optional raw)
  "Redisplay the current message.
With prefix argument (or when RAW is non-nil),
display the raw data of the message."
  (interactive "P")
  (unless (derived-mode-p 'bic-message-mode)
    (user-error "Not in message buffer"))
  (bic-message-display
   bic--current-account
   bic--current-mailbox
   bic-message--full-uid
   :raw (if raw t nil)))

(defun bic-message-toggle-header (&optional arg)
  "Show the headers if they are hidden, or hide them if they are shown.
If ARG is a positive number, show the entire header.
If ARG is a negative number, hide the unwanted header lines."
  (interactive "P")
  (cl-letf (((symbol-function 'gnus-set-mode-line) #'ignore))
    (gnus-summary-toggle-header arg)))

;;;###autoload
(defun bic-message-identify ()
  "Display the UID, mailbox and account of the current message.
The \"current\" message is the one displayed in a message buffer,
or the message under point in a mailbox buffer."
  (interactive)
  (let ((full-uid (bic--find-message-at-point)))
    (message "Message %s, in mailbox %s, account %s"
	     full-uid bic--current-mailbox bic--current-account)))

;;;###autoload
(defun bic-message-mark-read ()
  "Mark the message at point as read.
If the message is marked as flagged, remove the flag.
If the message is marked to be deleted, undelete it.

In a mailbox buffer, if the region is active, act on all messages in
the region."
  (interactive)
  (bic-message-flag-maybe-advance "read" '("\\Seen") '("\\Flagged" "\\Deleted")))

;;;###autoload
(defun bic-message-mark-unread ()
  "Mark the message at point as unread.
If the message is marked as flagged, remove the flag.
If the message is marked to be deleted, undelete it.

In a mailbox buffer, if the region is active, act on all messages in
the region."
  (interactive)
  (bic-message-flag-maybe-advance "unread" () '("\\Seen" "\\Flagged" "\\Deleted")))

;;;###autoload
(defun bic-message-mark-flagged ()
  "Mark the message at point as flagged.
Also mark it as read.
If the message is marked to be deleted, undelete it.

In a mailbox buffer, if the region is active, act on all messages in
the region."
  (interactive)
  (bic-message-flag-maybe-advance "flagged" '("\\Seen" "\\Flagged") '("\\Deleted")))

;;;###autoload
(defun bic-message-mark-spam ()
  "Mark the message at point as spam (junk).

In a mailbox buffer, if the region is active, act on all messages in
the region."
  (interactive)
  (bic-message-flag-maybe-advance "spam" '("$Junk") '("$NotJunk")))

;;;###autoload
(defun bic-message-mark-not-spam ()
  "Mark the message at point as not spam (not junk).

In a mailbox buffer, if the region is active, act on all messages in
the region."
  (interactive)
  (bic-message-flag-maybe-advance "not spam" '("$NotJunk") '("$Junk")))

;;;###autoload
(defun bic-message-mark-deleted ()
  "Mark the message at point for deletion.

In a mailbox buffer, if the region is active, act on all messages in
the region."
  (interactive)
  (bic-message-flag-maybe-advance "deleted" '("\\Deleted") ()))

;;;###autoload
(defun bic-message-flag (flags-to-add flags-to-remove &optional full-uid)
  "Add and remove flags for the message at point.
FLAGS-TO-ADD and FLAGS-TO-REMOVE are lists of strings.
If FULL-UID is specified, use that message instead of
the message at point."
  (let ((full-uid (or full-uid (bic--find-message-at-point)))
	(fsm (bic--find-account bic--current-account)))
    (fsm-send
     fsm
     (list :flags bic--current-mailbox full-uid flags-to-add flags-to-remove))))

(defun bic-message-flag-maybe-advance (human-readable flags-to-add flags-to-remove)
  "Add and remove flags, and maybe advance to next message.
If point is in a mailbox buffer (not a message buffer),
move point to the next message.
HUMAN-READABLE is a string to be used when prompting to confirm.
FLAGS-TO-ADD and FLAGS-TO-REMOVE are lists of strings.

If in a mailbox buffer and the region is active, act on all
messages in the region."
  (cond
   ((and (derived-mode-p 'bic-mailbox-mode) (use-region-p))
    (let* ((first (ewoc-locate bic-mailbox--ewoc (region-beginning)))
	   (last (ewoc-locate bic-mailbox--ewoc (1- (region-end)) first))
	   nodes
	   (count 0))
      (unless (and first last)
	(user-error "No message at point or mark"))
      (while last
	(push last nodes)
	(cl-incf count)
	(setq last (when (not (eq last first))
		     (ewoc-prev bic-mailbox--ewoc last))))
      (unless (yes-or-no-p (format "Mark %d messages as %s? " count human-readable))
	(signal 'quit nil))
      (setq deactivate-mark t)
      (dolist (node nodes)
	(bic-message-flag flags-to-add flags-to-remove (ewoc-data node)))))
   (t
    (bic-message-flag flags-to-add flags-to-remove)
    (when (derived-mode-p 'bic-mailbox-mode)
      (ignore-errors (ewoc-goto-next bic-mailbox--ewoc 1))))))

(defun bic-message-reply (&optional wide)
  "Compose a reply to the current message.
If WIDE is non-nil, address the reply to all recipients
as well as the sender of the original message (known as
\"reply all\" in other email clients)."
  (interactive)
  (unless (derived-mode-p 'bic-message-mode)
    (user-error "Not in message buffer"))
  (let ((full-uid (bic--find-message-at-point))
	(mailbox bic--current-mailbox)
	(account bic--current-account))
    (let (gnus-buffers)			;don't ask :(
      (gnus-copy-article-buffer gnus-original-article-buffer))
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

(provide 'bic-message)
;;; bic-message.el ends here
