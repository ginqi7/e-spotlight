;;; e-spotlight-calculator.el ---                     -*- lexical-binding: t; -*-

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

(defclass e-spotlight-calculator (e-spotlight-plugin)
  ())

(defvar e-spotlight-calculator--icon-path
  (file-name-concat
   (file-name-directory (or load-file-name
                            (buffer-file-name)))
   "icon.png"))


(defun e-spotlight-calculator-copy (data)
  (kill-new (gethash "value" data)))


(defun e-spotlight-calculator-command (keyword)
  (map-into (list (cons "exp"  keyword)
                  (cons "value"  (format "%s"(calc-eval keyword))))
            '(hash-table :test equal)))

(defun e-spotlight-calculator-parse (data)
  (list (e-spotlight-candidate
         :key (gethash "exp" data)
         :icon (create-image e-spotlight-calculator--icon-path nil nil :width e-spotlight-icon-width)
         :value data
         :action #'e-spotlight-calculator-copy
         :preview (gethash "value" data))))

(add-to-list 'e-spotlight-plugins
             (e-spotlight-calculator
              :fetch #'e-spotlight-fetch
              :command #'e-spotlight-calculator-command
              :parse #'e-spotlight-calculator-parse))

(provide 'e-spotlight-calculator)
;;; e-spotlight-calculator.el ends here

