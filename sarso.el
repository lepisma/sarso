;;; sarso.el --- Functions for working with sarso db -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.3.2
;; Package-Requires: ((emacs "26") (helm "3.6.2") (emacsql "3.0.0") (emacsql-sqlite "3.0.0") (esi "0.0.5"))
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
(require 'esi)
(require 'esi-search)
(require 'helm)
(require 'org)
(require 's)
(require 'seq)

(defcustom sarso-db-path "~/.sarso.sqlite"
  "Path to sarso sqlite database.")

(defcustom sarso-jira-root nil
  "Root url for Jira. This looks something like this
https://company-name.atlassian.net")

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
         :documentation "Jira issue type."))
  "An issue in sarso database.")

(defun sarso-db-exec (sql)
  "Execute sql and return results."
  (with-temp-buffer
    (call-process "sqlite3" nil t nil (expand-file-name sarso-db-path) sql)
    (let ((sep "|"))
      (mapcar (lambda (line) (s-split sep line)) (s-split "\n" (s-trim (buffer-string)))))))

(defun sarso-read-issues ()
  "Return a list of sarso issues from database."
  ;; NOTE: We skip reading `description' as of now which means issues from here
  ;;       will have "" description.
  (let ((lines (sarso-db-exec "SELECT key, summary, type FROM issues")))
    (mapcar (lambda (line) (sarso-issue :key (nth 0 line)
                                   :summary (nth 1 line)
                                   :type (nth 2 line)))
            lines)))

(defun sarso-read-issue-types ()
  "Return a list of possible task types"
  (mapcar #'car (sarso-db-exec "SELECT DISTINCT type FROM issues")))

(cl-defmethod sarso-format-issue ((i sarso-issue))
  "Format issue I for helm display and completion."
  (format "%s: %s" (oref i :key) (oref i :summary)))

(cl-defmethod sarso-issue-link ((i sarso-issue))
  "Return Jira url for give issue I."
  (concat (file-name-as-directory sarso-jira-root) "browse/" (oref i :key)))

(cl-defmethod sarso-insert-issue-link ((i sarso-issue))
  "Insert org mode style link for given issue I."
  (org-insert-link nil (sarso-issue-link i) key))

(defun sarso-new-issue (summary &optional type)
  "Create and insert a new issue in the database."
  (interactive (list (read-from-minibuffer "Summary: ") (completing-read "Issue Type: " (sarso-read-issue-types) nil t)))
  (let* ((issue (sarso-issue :summary summary :type (or type "Task")))
         (sql (format "INSERT INTO issues(summary, description, type) VALUES(\"%s\", \"\", \"%s\")" (oref issue :summary) (oref issue :type))))
    (sarso-db-exec sql)))

;;;###autoload
(defun sarso-voice-search-open ()
  "Perform a voice search and open found issues in browser."
  (interactive)
  (let ((candidates (mapcar (lambda (i) (cons (downcase (oref i :summary)) i)) (sarso-read-issues)))
        (transcriptions (esi-transcribe-to-strings))
        (max-matches 10))
    (if (null transcriptions)
        (message "No results from transcription.")
      (message "Searching with %s" (car transcriptions))
      (let ((scored-candidates (esi-search-filter (esi-search-sort (car transcriptions) candidates))))
        (message "Found %d overall matches" (length scored-candidates))
        (mapcar (lambda (c) (browse-url-default-browser (sarso-issue-link (cdr c)))) (seq-take scored-candidates max-matches))))))

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
