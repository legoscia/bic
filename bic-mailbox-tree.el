;;; bic-mailbox-tree.el --- display tree of mailboxes  -*- lexical-binding: t; -*-

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

;; Use tree-widget to display known accounts and their mailboxes.

;;; Code:

(require 'tree-widget)

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

(defface bic-mailbox-tree-mailbox-unlimited-sync-unread
  '((t (:inherit gnus-group-mail-1)))
  "Face used for fully synced mailboxes with unread messages in mailbox tree."
  :group 'bic)

(defface bic-mailbox-tree-mailbox-unlimited-sync
  '((t (:inherit gnus-group-mail-1-empty)))
  "Face used for fully synced mailboxes in mailbox tree."
  :group 'bic)

(defface bic-mailbox-tree-mailbox-partial-sync-unread
  '((t (:inherit gnus-group-mail-low)))
  "Face used for partially synced mailboxes in mailbox tree."
  :group 'bic)

(defface bic-mailbox-tree-mailbox-partial-sync
  '((t (:inherit gnus-group-mail-low-empty)))
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

;;;###autoload
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
	     (attributes (plist-get (cdr mailbox-data) :attributes))
	     (sync-level (bic--infer-sync-level mailbox-data))
	     (unseen (plist-get (cdr mailbox-data) :unseen)))
	 ;; It's unclear whether these attributes are case sensitive
	 ;; or not, so let's use cl-equalp.
	 (if (or (cl-member "\\Noselect" attributes :test #'cl-equalp)
		 (cl-member "\\NonExistent" attributes :test #'cl-equalp))
	     (widget-convert 'item mailbox-name)
	   (widget-convert
	    'link
	    :account-name account-name
	    :mailbox-name (car mailbox-data)
	    :notify (lambda (widget &rest _ignore)
		      (bic-mailbox-open (widget-get widget :account-name)
					(widget-get widget :mailbox-name)))
	    :tag (if unseen (bic-number-to-string unseen) "?")
	    :format (concat
		     "%[%v%]"
		     (when sync-level " (%t)")
		     (cond
		      ((plist-get (cdr mailbox-data) :not-all-unread)
		       (propertize " [not all unread fetched]"
				   'face 'error))
		      ((plist-get (cdr mailbox-data) :not-all-recent)
		       (propertize " [not all recent fetched]"
				   'face 'warning)))
		     "\n")
	    :button-face (pcase (cons sync-level (and (numberp unseen) (not (zerop unseen))))
			   (`(:unlimited-sync . t)
			    'bic-mailbox-tree-mailbox-unlimited-sync-unread)
			   (`(:unlimited-sync . nil)
			    'bic-mailbox-tree-mailbox-unlimited-sync)
			   (`(:partial-sync . t)
			    'bic-mailbox-tree-mailbox-partial-sync-unread)
			   (`(:partial-sync . nil)
			    'bic-mailbox-tree-mailbox-partial-sync)
			   (_
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

(provide 'bic-mailbox-tree)
;;; bic-mailbox-tree.el ends here
