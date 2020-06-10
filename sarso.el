;;; sarso.el --- Functions for working with sarso db -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26") (helm "3.6.2") (emacsql "3.0.0") (emacsql-sqlite "3.0.0"))
;; URL: https://github.com/lepisma/sarso

;;; Commentary:

;; Functions for working with sarso db
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'eieio)
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'helm)
(require 'org)

(defcustom sarso-db-path "~/.sarso.sqlite"
  "Path to sarso sqlite database.")

(defcustom sarso-jira-root nil
  "Root url for Jira. This looks something like this
https://company-name.atlassian.net")

(defclass sarso-issue ()
  ((summary :initarg :summary
            :type string
            :documentation "Title of issue")
   (description :initarg :description
                :initform ""
                :type string
                :documentation "Detailed description"))
  "An issue in sarso database.")

(defun sarso-read-issues ()
  "Return a list of issues from database."
  (let ((db (emacsql-sqlite sarso-db-path)))
    (emacsql db [:select [key summary] :from issues])))

(defun sarso-format-issue (i)
  "Format issue I for helm display and completion."
  (format "%s: %s" (car i) (cdr i)))

(defun sarso-insert-issue-link (i)
  "Insert org mode style link for given issue I."
  (let* ((key (symbol-name (car i)))
         (url (concat (file-name-as-directory sarso-jira-root) "browse/" key)))
    (org-insert-link nil url key)))

;;;###autoload
(defun sarso-insert-link ()
  "Prompt for summary search and insert links in org mode style."
  (interactive)
  (let ((issues (sarso-read-issues)))
    (helm :sources (helm-build-sync-source "sarso issues"
                     :candidates (mapcar (lambda (i) (cons (sarso-format-issue i) i)) issues)
                     :action #'sarso-insert-issue-link)
          :buffer "*helm sarso insert link*")))

(provide 'sarso)

;;; sarso.el ends here
