;;; sarso.el --- Functions for working with sarso db -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.4.0
;; Package-Requires: ((emacs "26") (helm "3.7.1") (s "1.12.0"))
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

(require 'cl-lib)
(require 'eieio)
(require 'helm)
(require 'org)
(require 's)
(require 'seq)

(defcustom sarso-db-path "~/.sarso.sqlite"
  "Path to sarso sqlite database.")

(defcustom sarso-jira-root nil
  "Root url for Jira. This looks something like this
https://company-name.atlassian.net")

(defcustom sarso-jira-user nil
  "User name for Jira.")

(defcustom sarso-jira-token nil
  "Token / password for Jira.")

(defcustom sarso-sync-projects nil
  "List of projects to sync locally.")

(defcustom sarso-self-email nil
  "Email id of the account to have the default association.")

(defvar sarso-command "sarso"
  "Name of the command line variable.")

(defvar sarso-post-sync-hook nil
  "Hook called after a sync process is done.")

(defclass sarso-user ()
  ((account-id :initarg :account-id
               :type string
               :documentation "User Jira account id.")
   (email :initarg :email
          :type string)
   (display-name :initarg :display-name
                 :type string))
  "A Jira User.")

(defclass sarso-issue ()
  ((key :initarg :key
        :initform nil
        :documentation "Jira Key of the issue. This also sorta of
        maintains the project name.")
   (summary :initarg :summary
            :type string
            :documentation "Title of issue")
   (description :initarg :description
                :initform ""
                :type string
                :documentation "Detailed description")
   (type :initarg :type
         :type string
         :documentation "Jira issue type.")
   (assignee :initarg :assignee
             :type (or null sarso-user))
   (due-date :initarg :due-date
             :initform nil))
  "An issue in sarso database.")

(defun kebab-case-to-env-case (name)
  (s-replace "-" "_" (upcase name)))

(defmacro with-env (spec &rest body)
  "Evaluate BODY with environment variable set.

\(fn ((VAR VAL) (VAR VAL)) BODY...)"
  (let ((env-pairs (mapcar (lambda (var-val) (cons (kebab-case-to-env-case (symbol-name (car var-val))) (nth 1 var-val))) spec)))
    `(let ((old-pairs (mapcar (lambda (pair) (cons (car pair) (getenv (car pair)))) ',env-pairs)))
       (unwind-protect
           (progn (dolist (pair ',env-pairs)
                    (setenv (car pair) (cdr pair)))
                  ,@body)
         (dolist (pair old-pairs)
           (setenv (car pair) (cdr pair)))))))

(defun sarso-db-exec (sql)
  "Execute sql and return results."
  (with-temp-buffer
    (call-process "sqlite3" nil t nil (expand-file-name sarso-db-path) sql)
    (let ((sep "|"))
      (mapcar (lambda (line) (s-split sep line)) (s-split "\n" (s-trim (buffer-string)))))))

(defun sarso-read-users ()
  "Return a list of sarso users from database."
  (let ((lines (sarso-db-exec "SELECT account_id, email, display_name FROM users")))
    (mapcar (lambda (line) (sarso-user :account-id (nth 0 line)
                                  :email (nth 1 line)
                                  :display-name (nth 2 line)))
            lines)))

(defun sarso-parse-datetime (dt-string)
  "Parse datetime from sarso db."
  (let ((dt (iso8601-parse dt-string)))
    (unless (equal dt '(0 0 0 1 1 1 nil nil 0))
      dt)))

(defun sarso-read-issues ()
  "Return a list of sarso issues from database."
  ;; NOTE: We skip reading `description' as of now which means issues from here
  ;;       will have "" description.
  (let ((users (sarso-read-users))
        (lines (sarso-db-exec "SELECT key, summary, type, assignee_id, duedate FROM issues")))
    (mapcar (lambda (line) (sarso-issue :key (nth 0 line)
                                   :summary (nth 1 line)
                                   :type (nth 2 line)
                                   :assignee (find (nth 3 line) users :key (lambda (o) (oref o :account-id)) :test 'equal)
                                   :due-date (sarso-parse-datetime (nth 4 line))))
            lines)))

(cl-defmethod sarso-format-issue ((i sarso-issue))
  "Format issue I for helm display and completion."
  (format "%s: %s" (oref i :key) (oref i :summary)))

(cl-defmethod sarso-issue-link ((i sarso-issue))
  "Return Jira url for give issue I."
  (concat (file-name-as-directory sarso-jira-root) "browse/" (oref i :key)))

(provide 'sarso)

;;; sarso.el ends here
