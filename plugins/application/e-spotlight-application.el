;;; e-spotlight-application.el ---                   -*- lexical-binding: t; -*-

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

(defclass e-spotlight-application (e-spotlight-plugin)
  ())

(defclass e-spotlight-application-actions (e-spotlight-application)
  ((application :initarg :application)))

(defun e-spotlight-application-actions-parse (data)
  (let ((table (map-into
                (list (cons (format "Active %s" (gethash "_name" data)) #'e-spotlight-application-open)
                      (cons (format "Quit %s" (gethash "_name" data)) #'e-spotlight-application-quit)
                      (cons (format "Trash %s" (gethash "_name" data)) #'e-spotlight-application-trash))
                '(hash-table :test equal))))
    (maphash (lambda (key value)
               (puthash key (e-spotlight-candidate
                             :key key
                             :value data
                             :action value)
                        table))
             table)
    table))


(defun e-spotlight-application-get-icon (path)
  (e-spotlight-icon-get "applications"
                        path
                        "png"
                        (lambda () (e-spotlight-application-download-icon path))))

(defun e-spotlight-application-preview (data)
  (format "[Info]: %s\n[Path]: %s" (gethash "info" data) (gethash "path" data)))

(defun e-spotlight-application-parse (output)
  (let ((table (make-hash-table :test #'equal))
        (json (gethash "SPApplicationsDataType" (json-parse-string output :array-type 'list))))
    (dolist (item json)
      (puthash (gethash "_name" item)
               (e-spotlight-candidate
                :icon (e-spotlight-application-get-icon (gethash "path" item))
                :key (gethash "_name" item)
                :value item
                :plugins (list (e-spotlight-application-actions :fetch #'e-spotlight-fetch
                                                                :command (lambda (_) item)
                                                                :parse #'e-spotlight-application-actions-parse
                                                                :filter #'e-spotlight--filter))
                :action #'e-spotlight-application-open
                :preview #'e-spotlight-application-preview)
               table))
    table))

(defun e-spotlight-application-open (data)
  (shell-command-to-string (format "open -a '%s'" (gethash "path" data))))

(defun e-spotlight-application-trash (data)
  (shell-command-to-string (format "trash '%s'" (gethash "path" data))))

(defun e-spotlight-application-quit (data)
  (shell-command-to-string (format "pkill -9 -f '%s'" (gethash "path" data))))

(defun e-spotlight-application-download-icon (app-path)
  (e-spotlight-process-queue
   "e-spotlight-application-download-icon"
   "sh"
   e-spotlight-application-get-icon-shell-path
   app-path
   (e-spotlight-icon-file-name "applications" app-path "png"))
  nil)

(add-to-list 'e-spotlight-plugins (e-spotlight-application
                                   :fetch #'e-spotlight-process-async-run-shell
                                   :command "system_profiler SPApplicationsDataType -json"
                                   :ttl (* 30 60)
                                   :parse #'e-spotlight-application-parse
                                   :filter #'e-spotlight--filter))

(add-to-list 'e-spotlight-plugin-prefix
             '(e-spotlight-application . "app"))

(provide 'e-spotlight-application)
;;; e-spotlight-application.el ends here

