;;; sarso.el --- Functions for working with sarso db -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 1.1.0
;; Package-Requires: ((emacs "26") (async "1.9.4") (helm "3.7.1") (s "1.12.0"))
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

(require 'async)
(require 'cl-lib)
(require 'eieio)
(require 'helm)
(require 'json)
(require 'org)
(require 's)
(require 'seq)

(defcustom sarso-db-path "~/.sarso.sqlite"
  "Path to sarso sqlite database.")

(defcustom sarso-org-sink-file nil
  "Path of the Org file to put self assigned items in.")

(defcustom sarso-jira-root nil
  "Root url for Jira. This looks something like this
https://company-name.atlassian.net")

(defcustom sarso-jira-user nil
  "User name for Jira.")

(defcustom sarso-jira-token nil
  "Token / password for Jira.")

(defcustom sarso-sync-projects nil
  "List of project keys to sync locally.")

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
   (status :initarg :status
           :type string)
   (resolution :initarg :resolution
               :type (or null string))
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
  (declare (indent defun))
  `(let* ((env-pairs (mapcar (lambda (var-val) (cons (kebab-case-to-env-case (symbol-name (car var-val))) (eval (nth 1 var-val)))) ',spec))
          (old-pairs (mapcar (lambda (pair) (cons (car pair) (getenv (car pair)))) env-pairs)))
     (unwind-protect
         (progn (dolist (pair env-pairs)
                  (setenv (car pair) (cdr pair)))
                ,@body)
       (dolist (pair old-pairs)
         (setenv (car pair) (cdr pair))))))

;;;###autoload
(defun sarso-sync ()
  "Run sarso sync."
  (interactive)
  (with-env ((jira-base sarso-jira-root)
             (jira-user sarso-jira-user)
             (jira-token sarso-jira-token))
    (apply #'async-start-process "sarso" sarso-command
             (lambda (proc) (run-hooks 'sarso-post-sync-hook))
             "sync" sarso-sync-projects)))

(defun sarso-db-exec (sql)
  "Execute sql and return results."
  (with-temp-buffer
    (call-process "sqlite3" nil t nil (expand-file-name sarso-db-path) sql "-json")
    (goto-char (point-min))
    (json-parse-buffer :object-type 'plist :array-type 'list :null-object nil)))

(defun sarso-read-users ()
  "Return a list of sarso users from database."
  (mapcar (lambda (rec) (sarso-user :account-id (plist-get rec :account_id)
                               :email (plist-get rec :email)
                               :display-name (plist-get rec :display_name)))
          (sarso-db-exec "SELECT account_id, email, display_name FROM users")))

(defun sarso-parse-datetime (dt-string)
  "Parse datetime from sarso db and return a Lisp timestamp."
  (let ((dt (iso8601-parse dt-string)))
    (unless (equal dt '(0 0 0 1 1 1 nil nil 0))
      (encode-time dt))))

(defun sarso-read-issues ()
  "Return a list of sarso issues from database."
  ;; NOTE: We skip reading `description' as of now which means issues from here
  ;;       will have "" description.
  (let ((users (sarso-read-users)))
    (mapcar (lambda (rec) (sarso-issue :key (plist-get rec :key)
                                  :summary (plist-get rec :summary)
                                  :type (plist-get rec :type)
                                  :status (plist-get rec :status)
                                  :resolution (plist-get rec :resolution)
                                  :assignee (find (plist-get rec :assignee_id) users :key (lambda (o) (oref o :account-id)) :test 'equal)
                                  :due-date (sarso-parse-datetime (plist-get rec :duedate))))
            (sarso-db-exec "SELECT key, summary, type, status, resolution, assignee_id, duedate FROM issues"))))

(cl-defmethod sarso-issue-self-p ((i sarso-issue))
  "Tell whether the issue is assigned to me."
  (unless sarso-self-email
    (error "`sarso-self-email' not set"))
  (when (oref i :assignee)
    (string-equal (oref (oref i :assignee) :email) sarso-self-email)))

(cl-defmethod sarso-issue-resolved-p ((i sarso-issue))
  "Tell if the issue is resolved."
  (or (member (oref i :status) '("Done"))
      (member (oref i :resolution) '("Done"))))

(cl-defmethod sarso-issue-link ((i sarso-issue))
  "Return Jira url for give issue I."
  (concat (file-name-as-directory sarso-jira-root) "browse/" (oref i :key)))

(cl-defmethod sarso-issue-org-format ((i sarso-issue))
  "Format the issue as an org mode headline."
  (with-temp-buffer
    (org-mode)
    (org-insert-heading)
    (insert (oref i :summary))
    (if (oref i :due-date)
        (org-deadline nil (format-time-string "%F" (oref i :due-date)))
      (org-schedule nil (format-time-string "%F")))
    (org-set-property "JIRA-URL" (sarso-issue-link i))
    (when (oref i :assignee)
      (org-set-property "ASSIGNEE" (oref (oref i :assignee) :display-name)))
    (buffer-substring-no-properties (point-min) (point-max))))

;;;###autoload
(defun sarso-self-issues-to-org ()
  "Save self assigned issues to org sink."
  (unless sarso-org-sink-file
    (error "`sarso-org-sink-file' not set."))
  (let ((issues (cl-remove-if-not (lambda (i) (and (sarso-issue-self-p i) (not (sarso-issue-resolved-p i)))) (sarso-read-issues))))
    (with-current-buffer (find-file-noselect sarso-org-sink-file)
      (erase-buffer)
      (dolist (i issues)
        (insert (sarso-issue-org-format i) "\n"))
      (save-buffer))))

(provide 'sarso)

;;; sarso.el ends here
