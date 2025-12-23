;;; e-spotlight-cache.el ---                         -*- lexical-binding: t; -*-

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

(defvar e-spotlight-cache (make-hash-table :test #'equal))

(defun e-spotlight-cache--current-time ()
  (let ((current-time-list nil))
    (/ (car (current-time)) (cdr (current-time)))))

(defun e-spotlight-cache-put (name key value ttl)
  (let ((repository (gethash name e-spotlight-cache)))
    (unless repository
      (puthash name (make-hash-table :test #'equal) e-spotlight-cache)
      (setq repository (gethash name e-spotlight-cache)))
    (puthash key (cons value (+ (e-spotlight-cache--current-time) ttl)) repository)))

(defun e-spotlight-cache-get (name key)
  (unless e-spotlight-cache
    (setq e-spotlight-cache (make-hash-table :test #'equal)))
  (when-let* ((repository (gethash name e-spotlight-cache))
              (value (gethash key repository)))
    (if (> (cdr value) (e-spotlight-cache--current-time))
        (car value)
      (e-spotlight-cache-remove name key))))

(defun e-spotlight-cache-remove (name key)
  (when-let* ((repository (gethash name e-spotlight-cache)))
    (remhash key repository)
    (when (hash-table-empty-p repository)
      (remhash name e-spotlight-cache))))


(provide 'e-spotlight-cache)
;;; e-spotlight-cache.el ends here
