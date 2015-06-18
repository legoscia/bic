;;; bic-bbdb.el --- BBDB bindings for BIC            -*- lexical-binding: t; -*-

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

;; Bind `:' in bic-message-mode to bring up BBDB records for the
;; current message.
;;
;; Insinuate BBDB to understand what BIC is about.

;;; Code:

;; Defined in bbdb-mua.
(defvar bbdb-mua-mode-alist)

;;;###autoload
(with-eval-after-load "bbdb-mua"
  ;; Need to tell BBDB what kind of mail client we are.
  ;; `message' is close enough, so add `bic-message-mode' as one of
  ;; the major modes that trigger "message" treatment.
  (let ((message-entry (assq 'message bbdb-mua-mode-alist)))
    (cl-pushnew 'bic-message-mode (cdr message-entry))))

;;;###autoload
(with-eval-after-load "bic-message"
  (define-key bic-message-mode-map ":" 'bbdb-mua-display-all-records))

(provide 'bic-bbdb)
;;; bic-bbdb.el ends here
