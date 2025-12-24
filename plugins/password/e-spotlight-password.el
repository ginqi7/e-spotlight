;;; e-spotlight-password.el ---                      -*- lexical-binding: t; -*-

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

(defclass e-spotlight-password (e-spotlight-plugin)
  (password :initarg :password))

(defclass e-spotlight-password-actions (e-spotlight-password)
  ((application :initarg :application)))

(defun e-spotlight-password-actions-parse (output)
  (let* ((data (json-parse-string output :array-type 'list))
         (table (map-into
                 (list (cons (format "Copy %s user name." (gethash "name" data))
                             (lambda (_) (kill-new (gethash "username" (gethash "data" data)))))
                       (cons (format "Copy %s password." (gethash "name" data))
                             (lambda (_) (kill-new (gethash "password" (gethash "data" data))))))
                 '(hash-table :test equal))))
    (maphash (lambda (key value)
               (puthash key (e-spotlight-candidate
                             :key key
                             :value data
                             :action value)
                        table))
             table)
    table))

(defun e-spotlight-password-parse (output)
  (let ((table (make-hash-table :test #'equal))
        (json (json-parse-string output :array-type 'list)))
    (dolist (item json)
      (puthash (gethash "name" item)
               (e-spotlight-candidate
                :key (gethash "name" item)
                :value item
                :plugins (list (e-spotlight-password-actions
                                :fetch #'e-spotlight-process-async-run-shell
                                :command (lambda (_) (format "rbw get %s --raw" (gethash "id" item)))
                                :ttl 1
                                :parse #'e-spotlight-password-actions-parse
                                :filter #'e-spotlight--filter))
                :preview (lambda (item) (format "user: %s" (gethash "user" item))))
               table))
    table))

(defun e-spotlight-password-command (str)
  (format "rbw search %s --raw" str))


(add-to-list 'e-spotlight-plugins (e-spotlight-password
                                   :fetch #'e-spotlight-process-async-run-shell
                                   :command #'e-spotlight-password-command
                                   :ttl 1
                                   :parse #'e-spotlight-password-parse
                                   :filter #'e-spotlight--filter))


(add-to-list 'e-spotlight-plugin-prefix '(e-spotlight-password . "pass"))

(provide 'e-spotlight-password)
;;; e-spotlight-password.el ends here
