;;; e-spotlight-icon.el ---                          -*- lexical-binding: t; -*-

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

(defcustom e-spotlight-icon-directory
  (locate-user-emacs-file "e-spotlight-icons")
  "Directory to store cached icons."
  :type 'directory)

(defcustom e-spotlight-icon-width 24
  "The width of icons in pixels."
  :type 'integer)

(defun e-spotlight-icon-file-name (plugin-name identity-str file-type)
  "Get icon image. If not exists, ensure dir exists and call DOWNLOAD-FUNC with file path."
  (file-name-concat e-spotlight-icon-directory
                    plugin-name
                    (file-name-with-extension (md5 identity-str) file-type)))

(defun e-spotlight-icon-get (plugin-name id ext &optional download-func)
  "Get icon image. If not exists, ensure dir exists and call DOWNLOAD-FUNC with file path."
  (let ((file (e-spotlight-icon-file-name plugin-name id ext)))
    (if (file-exists-p file)
        (create-image file nil nil :width e-spotlight-icon-width)
      (when download-func
        (make-directory (file-name-directory file) t)
        (funcall download-func)))))

(provide 'e-spotlight-icon)
;;; e-spotlight-icon.el ends here
