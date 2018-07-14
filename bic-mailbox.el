;;; bic-mailbox.el --- list messages in a mailbox    -*- lexical-binding: t; -*-

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

;; Display messages in a mailbox using an ewoc.

;;; Code:

(require 'ewoc)

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

(defvar-local bic-mailbox--ewoc nil)

(defvar-local bic-mailbox--ewoc-nodes-table nil
  "Hash table mapping uidvalidity+uid to ewoc nodes.")

(defvar-local bic-mailbox--hashtable nil)

(defvar-local bic-mailbox--flags-table nil)

(defvar-local bic-mailbox--uid-tree nil)

(defvar-local bic-mailbox--fixup-times-timer nil)

(defvar-local bic-mailbox--fixup-times-at nil)

;;;###autoload
(defun bic-mailbox-open (account mailbox)
  ;; checkdoc-order: nil
  "Open MAILBOX in a mailbox buffer.
MAILBOX is the name of a mailbox belonging to ACCOUNT."
  (interactive
   (let* ((account (bic--read-existing-account "IMAP account: " t))
	  (mailbox (bic--read-mailbox "Mailbox: " account t)))
     (list account mailbox)))
  (let ((buffer-name (concat (utf7-decode mailbox t) "-" account)))
    (with-current-buffer (get-buffer-create buffer-name)
      (if (derived-mode-p 'bic-mailbox-mode)
	  ;; If we already have a mailbox buffer for this mailbox,
	  ;; ensure that it's up to date.
	  (bic-mailbox-update)
	(bic-mailbox-mode)
	(bic-mailbox--init account mailbox)))
    (switch-to-buffer buffer-name)))

;;;###autoload
(defun bic-mailbox--find-buffer (account mailbox)
  ;; checkdoc-order: nil
  "Return the buffer viewing MAILBOX for ACCOUNT.
If there is no such buffer, return nil."
  (get-buffer (concat (utf7-decode mailbox t) "-" account)))

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
    (define-key map "n" 'bic-mailbox-next-unread)
    (define-key map " " 'bic-mailbox-next-page-or-next-unread)
    (define-key map "=" 'bic-message-identify)
    map))

(define-derived-mode bic-mailbox-mode special-mode "BIC mailbox"
  "Major mode for IMAP mailboxes accessed by `bic'."
  (setq header-line-format
	'(" " bic--current-account
	  " " (:eval (utf7-decode bic--current-mailbox t))
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
  ;; checkdoc-params: (account mailbox)
  "Initialise a new mailbox buffer."
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
  "Add list of messages to mailbox buffer."
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
  ;; checkdoc-params: (msg)
  "Ewoc pretty-printer function for mailbox buffer."
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
  "Return the font face to use for a message, based on FLAGS."
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
  "Return indicators to use for a message, given FLAGS.
The indicators consist of two characters.  First character:

- If the message is marked as spam: $
- If the message is flagged: !
- If the message is read: R
- If the message is recent: .
- Otherwise, the first character is a space

Second character:

- If the message has been answered: A
- If the message has been forwarded: F
- Otherwise, the second character is a space"
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
  "Format DATE for mailbox message listing.
If the message was sent today, show the time as HH:MM.
If the message was sent within the last 180 days,
show the date as DD MMM (abbreviated month).
Otherwise, show a numeric date as YYYY-MM-DD."
  (let ((parsed-date (ignore-errors (date-to-time date))))
    (if (null date)
	;; cannot parse
	"**********"
      (let* ((now (time-to-days (current-time)))
	     (days (- now (time-to-days parsed-date))))
	(cond
	 ((= days 0)
	  ;; same day: show time
	  (prog1
	      (propertize (format-time-string "     %H:%M" parsed-date)
			  'bic-mailbox--timestamp-without-date now)
	    ;; Remember that we wrote a time-only date.
	    (unless (and bic-mailbox--fixup-times-at (<= bic-mailbox--fixup-times-at now))
	      ;; Need to fix this time tomorrow.
	      (setq bic-mailbox--fixup-times-at (1+ now)))
	    (bic-mailbox--fixup-times-maybe-start-timer)))
	 ((< days 180)
	  ;; less than half a year ago: show date without year
	  (format "%10s" (format-time-string "%e %b" parsed-date)))
	 (t
	  ;; more than half a year ago, or in the future: show YYYY-MM-DD
	  (format-time-string "%F" parsed-date)))))))

(defun bic-mailbox--fixup-times (buffer)
  "At midnight, update message timestamps in BUFFER.
Any message that was showing just the time will be
updated to show the date instead, since it's already
from yesterday."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless (derived-mode-p 'bic-mailbox-mode)
	(error "Not a mailbox buffer"))
      (let ((now (time-to-days (current-time)))
	    (pos (point-min))
	    (node nil)
	    (have-more nil))
	(while (setq pos (next-single-property-change pos 'bic-mailbox--timestamp-without-date))
	  (let ((day (get-text-property pos 'bic-mailbox--timestamp-without-date)))
	    (when (numberp day)
	      (cond
	       ((< day now)
		(setq node (ewoc-locate bic-mailbox--ewoc pos node))
		(bic-mailbox--invalidate bic-mailbox--ewoc node))
	       ((= day now)
		(setq have-more t))))))
	(if have-more
	    (progn
	      (setq bic-mailbox--fixup-times-at (1+ now))
	      (bic-mailbox--fixup-times-maybe-start-timer))
	  (setq bic-mailbox--fixup-times-at nil
		bic-mailbox--fixup-times-timer nil))))))

(defun bic-mailbox--fixup-times-maybe-start-timer ()
  "Set timer for fixing message timestamps.
At midnight, we change HH:MM timestamps for messages sent
today into the format for messages sent earlier.
See `bic-mailbox--fixup-times'."
  (unless (or
	   ;; If there's already a timer that's set to run in the
	   ;; future, there's no need to set a new one.
	   (and (timerp bic-mailbox--fixup-times-timer)
		(> 0 (timer-until bic-mailbox--fixup-times-timer (current-time))))
	   ;; Also check that there's a time when the timer needs to
	   ;; be run.
	   (null bic-mailbox--fixup-times-at))
    (let* ((utc-midnight
	    ;; NB: `bic-mailbox--fixup-times-at' was returned from
	    ;; `time-to-days', which returns the number of days from
	    ;; year 1.  However, `days-to-time' expects as its argument
	    ;; the number of days since the epoch.  Thus, we need to
	    ;; adjust the value.

	    ;; That gives us UTC midnight of the given day.
	    (days-to-time (- bic-mailbox--fixup-times-at
			     (time-to-days 0))))
	  (local-midnight
	   ;; Then we need to subtract the time zone offset, to get
	   ;; the local midnight.
	   (time-subtract utc-midnight (or (car (current-time-zone utc-midnight)) 0))))
      (setq bic-mailbox--fixup-times-timer
	    (run-at-time
	     local-midnight
	     nil
	     'bic-mailbox--fixup-times (current-buffer))))))

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
	 (cl-incf count))
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
	    `(:ensure-up-to-date ,bic--current-mailbox :verbose t)))

(defun bic-mailbox-read-message (keep-unread)
  ;; checkdoc-params: (keep-unread)
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
      (bic-message-flag '("\\Seen") '() msg))))

;;;###autoload
(defun bic-mailbox-next-unread ()
  "Open the next unread message."
  (interactive)
  (let (mailbox-buffer current-node)
    (cond
     ((derived-mode-p 'bic-mailbox-mode)
      (setq current-node (ewoc-locate bic-mailbox--ewoc)
	    mailbox-buffer (current-buffer)))
     ((derived-mode-p 'bic-message-mode)
      (setq mailbox-buffer (bic-mailbox--find-buffer
			    bic--current-account bic--current-mailbox))
      (unless mailbox-buffer
	(user-error "Cannot find mailbox buffer for %s of %s"
		    bic--current-mailbox bic--current-account))
      (setq current-node
	    (gethash bic-message--full-uid
		     (buffer-local-value 'bic-mailbox--ewoc-nodes-table mailbox-buffer))))
     (t
      (user-error "Not in message or mailbox buffer")))
    (with-current-buffer mailbox-buffer
      (let ((next-node current-node))
	(while
	    (progn
	      (setq next-node (ewoc-next bic-mailbox--ewoc next-node))
	      (and next-node
		   (member "\\Seen"
			   (gethash (ewoc-data next-node) bic-mailbox--flags-table)))))
	(unless next-node
	  (user-error "No more unread messages"))
	(let ((mailbox-window (get-buffer-window mailbox-buffer)))
	  ;; Complicated dance to ensure that we advance point
	  ;; regardless of whether the mailbox buffer is visible or
	  ;; not.
	  (if mailbox-window
	      (with-selected-window mailbox-window
		(ewoc-goto-node bic-mailbox--ewoc next-node))
	    (ewoc-goto-node bic-mailbox--ewoc next-node)))
	(bic-mailbox-read-message nil)))))

;;;###autoload
(defun bic-mailbox-next-page-or-next-unread ()
  "Show next page of message.
If at the end of the message, show next unread message."
  (interactive)
  (let* ((message-buffer (get-buffer "*BIC-Message*"))
	 (message-window (and message-buffer (get-buffer-window message-buffer))))
    (if (null message-window)
	;; No message is being displayed; open the next one.
	(bic-mailbox-next-unread)
      ;; Message displayed; scroll or move to next.
      (with-selected-window message-window
	(when (gnus-article-next-page)
	  (bic-mailbox-next-unread))))))

;;;###autoload
(defun bic-mailbox--maybe-update-message (address mailbox full-uid)
  ;; checkdoc-params: (address mailbox full-uid)
  "Update how a certain message is displayed in its mailbox buffer.
If there is no mailbox buffer for the mailbox in question, do nothing."
  (pcase (bic-mailbox--find-buffer address mailbox)
    ((and (pred bufferp) mailbox-buffer)
     (run-with-idle-timer
      0.1 nil 'bic-mailbox--update-message mailbox-buffer full-uid))))

(defun bic-mailbox--update-message (buffer full-uid)
  "Update the display of a message in a mailbox buffer.
BUFFER is the buffer displaying the mailbox, and FULL-UID
is a string containing the uidvalidity and the uid of the
message.

If the message is currently not displayed, add it to the end
of the buffer.  If the message is displayed, call the ewoc
pretty-printer again to update display for new flags etc."
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
	 (bic-mailbox--invalidate bic-mailbox--ewoc node))))))

(defun bic-mailbox--invalidate (ewoc node)
  ;; checkdoc-params: (ewoc node)
  "Like `ewoc-invalidate', but ensure point doesn't move.
Assumes that the size of the entry won't change."
  (let ((old-point (point)))
    ;; We use an integer instead of a marker, because we don't
    ;; expect the size of the entry to change, and if point was
    ;; on this entry or immediately after it, we would lose the
    ;; precise position and instead go back to the start of the
    ;; entry if we used a marker.
    (unwind-protect
	(ewoc-invalidate ewoc node)
      (goto-char old-point))))

;;;###autoload
(defun bic-mailbox--maybe-remove-message (address mailbox full-uid)
  ;; checkdoc-params: (address mailbox full-uid)
  "Remove a message from its mailbox buffer.
If there is no buffer displaying the mailbox in question, do nothing."
  (pcase (bic-mailbox--find-buffer address mailbox)
    ((and (pred bufferp) mailbox-buffer)
     (run-with-idle-timer
      0.1 nil 'bic-mailbox--remove-message mailbox-buffer full-uid))))

(defun bic-mailbox--remove-message (buffer full-uid)
  "Remove a message from BUFFER.
FULL-UID is a string containing the uidvalidity and the uid of the
message."
  (with-current-buffer buffer
    (pcase (gethash full-uid bic-mailbox--ewoc-nodes-table)
      (`nil
       ;; Not found; nothing to do.
       )
      (node
       (when (ewoc-location node)
	 (let ((inhibit-read-only t))
	   (ewoc-delete bic-mailbox--ewoc node)))))))



(provide 'bic-mailbox)
;;; bic-mailbox.el ends here
