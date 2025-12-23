;;; e-spotlight-chrome-tab.el --- Global switch Google Chrome tab with emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords: lisp, tools

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

(defcustom e-spotlight-browser-app-name "Brave Browser" "")

(defvar e-spotlight-browser-directory (file-name-directory (or load-file-name (buffer-file-name))))

(defvar e-spotlight-browser-tab-list-jsa-path
  (expand-file-name "list-browser-tab.js" e-spotlight-browser-directory))

(defvar e-spotlight-browser-tab-active-jsa-path
  (expand-file-name "active-browser-tab.js" e-spotlight-browser-directory))

(defvar e-spotlight-browser-tab-close-jsa-path
  (expand-file-name "close-browser-tab.js" e-spotlight-browser-directory))


(defclass e-spotlight-browser-tab (e-spotlight-plugin)
  ())

(defclass e-spotlight-browser-tab-actions (e-spotlight-browser-tab)
  ())

(defun e-spotlight-browser-tab-active (data)
  (let ((command (format "osascript -l JavaScript %s '%s' %s"
                         e-spotlight-browser-tab-active-jsa-path
                         e-spotlight-browser-app-name
                         (gethash "id" data))))
    (print command)
    (shell-command-to-string command)))

(defun e-spotlight-browser-tab-close (data)
  (let ((command (format "osascript -l JavaScript %s '%s' %s"
                         e-spotlight-browser-tab-close-jsa-path
                         e-spotlight-browser-app-name
                         (gethash "id" data))))
    (print command)
    (shell-command-to-string command)))

(defun e-spotlight-browser-tab-copy-url (data)
  (kill-new (gethash "url" data)))

(defun e-spotlight-browser-tab-actions-parse (data)
  (let* ((table (map-into
                 (list (cons (format "Active") #'e-spotlight-browser-tab-active)
                       (cons (format "Close") #'e-spotlight-browser-tab-close)
                       (cons (format "Copy URL %s" (gethash "url" data)) #'e-spotlight-browser-tab-copy-url))
                 '(hash-table :test equal))))
    (maphash (lambda (key value)
               (puthash key (e-spotlight-candidate
                             :key key
                             :value data
                             :action value)
                        table))
             table)
    table))

(defun e-spotlight-browser-tab-parse (output)
  (let ((table (make-hash-table :test #'equal))
        (json (json-parse-string output :array-type 'list)))
    (dolist (item json)
      (puthash (gethash "title" item)
               (e-spotlight-candidate
                :key (gethash "title" item)
                :value item
                :plugins (list (e-spotlight-browser-tab-actions
                                :fetch #'e-spotlight-fetch
                                :command (lambda (_) item)
                                :parse #'e-spotlight-browser-tab-actions-parse
                                :filter #'e-spotlight--filter))
                :action #'e-spotlight-browser-tab-active
                :preview (lambda (item) (format "[title]: %s\n[url]: %s"
                                                (gethash "title" item)
                                                (gethash "url" item))))
               table))
    table))

(defun e-spotlight-browser-tab-command (str)
  (format "%s -l JavaScript %s '%s'"
          (executable-find "osascript")
          e-spotlight-browser-tab-list-jsa-path
          e-spotlight-browser-app-name))


(add-to-list 'e-spotlight-plugins (e-spotlight-browser-tab
                                   :fetch #'e-spotlight-process-async-run-shell
                                   :command #'e-spotlight-browser-tab-command
                                   :ttl 1
                                   :parse #'e-spotlight-browser-tab-parse
                                   :filter #'e-spotlight--filter))


(provide 'e-spotlight-browser-tab)
;;; e-spotlight-browser-tab.el ends here
