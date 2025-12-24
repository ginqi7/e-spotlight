;;; e-spotlight-github.el ---                        -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:


(require 'e-spotlight)

(defvar e-spotlight-application-get-icon-shell-path
  (file-name-concat
   (file-name-directory (or load-file-name
                            (buffer-file-name)))
   "e-spotlight-application-get-icon.sh"))

(defclass e-spotlight-github-issue (e-spotlight-plugin)
  ())

(defclass e-spotlight-github-repository (e-spotlight-plugin)
  ())

(defclass e-spotlight-github-issue-actions (e-spotlight-github-issue)
  ())

(defclass e-spotlight-github-repository-actions (e-spotlight-github-repository)
  ())


(defun e-spotlight-github-issue-parse (output)
  (let ((table (make-hash-table :test #'equal))
        (json (json-parse-string output :array-type 'list)))
    (dolist (item json)
      (puthash (gethash "title" item)
               (e-spotlight-candidate
                :key (gethash "title" item)
                :value item
                :action #'e-spotlight-github-browse-url
                :preview (lambda (item) (format "user: %s" (gethash "title" item))))
               table))
    table))

(defun e-spotlight-github-browse-url (data)
  (browse-url (gethash "url" data)))

(defun e-spotlight-github-repository-parse (output)
  (let ((table (make-hash-table :test #'equal))
        (json (json-parse-string output :array-type 'list)))
    (dolist (item json)
      (puthash (gethash "name" item)
               (e-spotlight-candidate
                :key (gethash "name" item)
                :value item
                :action #'e-spotlight-github-browse-url
                :preview (lambda (item) (format "[user]: %s\n[desc]:%s"
                                                (gethash "name" item)
                                                (gethash "description" item))))
               table))
    table))


(defun e-spotlight-github-repository-command (_)
  (format "gh repo list --json name,description,url | cat"))

(defun e-spotlight-github-issue-command (_)
  (format "gh search issues --assignee @me --state open --json repository,url,title -q '[.[] | {url: .url, number: .number, labels: .labels[0].name, title: .title}]' | cat"))


(add-to-list 'e-spotlight-plugins (e-spotlight-github-repository
                                   :fetch #'e-spotlight-process-async-run-shell
                                   :command #'e-spotlight-github-repository-command
                                   :ttl 1
                                   :parse #'e-spotlight-github-repository-parse
                                   :filter #'e-spotlight--filter))

(add-to-list 'e-spotlight-plugins (e-spotlight-github-issue
                                   :fetch #'e-spotlight-process-async-run-shell
                                   :command #'e-spotlight-github-issue-command
                                   :ttl 1
                                   :parse #'e-spotlight-github-issue-parse
                                   :filter #'e-spotlight--filter))



(add-to-list 'e-spotlight-plugin-prefix '(e-spotlight-github-issue . "ghi"))
(add-to-list 'e-spotlight-plugin-prefix '(e-spotlight-github-repository . "ghr"))

(provide 'e-spotlight-github)
;;; e-spotlight-github.el ends here
