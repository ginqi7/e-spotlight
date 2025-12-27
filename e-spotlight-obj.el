;;; e-spotlight-obj.el ---                           -*- lexical-binding: t; -*-

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

(defclass e-spotlight-candidate ()
  ((icon :initarg :icon)
   (key :initarg :key)
   (value :initarg :value)
   (preview :initarg :preview)
   (action :initarg :action)
   (plugins :initarg :plugins)))

(defclass e-spotlight-input ()
  ((separator :initarg :separator)
   (prefix :initarg :prefix)
   (keyword :initarg :keyword)
   (raw :initarg :raw)
   (prev :initarg :prev)
   (updated :initarg :updated)))

(defclass e-spotlight-plugin ()
  ((fetch :initarg :fetch)
   (data :initarg data)
   (command :initarg :command)
   (parse :initarg :parse)
   (ttl :initarg :ttl)
   (filter :initarg :filter)
   (type :initarg :type)))

(defun e-spotlight-obj-get (obj slot)
  (and obj
       (slot-boundp obj slot)
       (eieio-oref obj slot)))

(defun e-spotlight-fetch (name command ttl callback)
  (funcall callback command))

(cl-defmethod e-spotlight-plugin-candidates ((plugin e-spotlight-plugin) (input e-spotlight-input))
  (let* ((plugin-name (class-name (object-class plugin)))
         (plugin-prefix (alist-get plugin-name e-spotlight-plugin-prefix))
         (prefix (e-spotlight-obj-get e-spotlight--input 'prefix))
         (keyword (e-spotlight-obj-get e-spotlight--input 'keyword))
         (fetch (e-spotlight-obj-get plugin 'fetch))
         (command (if (stringp (e-spotlight-obj-get plugin 'command))
                      (e-spotlight-obj-get plugin 'command)
                    (funcall (e-spotlight-obj-get plugin 'command) keyword)))
         (ttl (e-spotlight-obj-get plugin 'ttl))
         (parse (e-spotlight-obj-get plugin 'parse))
         (filter (e-spotlight-obj-get plugin 'filter)))
    (when (or (not prefix)
              (string= prefix plugin-prefix))
      (funcall fetch
               (symbol-name (class-name (object-class plugin)))
               command
               ttl
               (lambda (output)
                 (when parse
                   (setq output (funcall parse output)))
                 (when filter
                   (setq output (funcall filter keyword output)))
                 (setq e-spotlight--candidates (append e-spotlight--candidates output))
                 (e-spotlight-update))))))


(cl-defmethod e-spotlight-candidate-run ((candidate e-spotlight-candidate))
  (when-let* ((action (e-spotlight-obj-get candidate 'action)))
    (funcall action (e-spotlight-obj-get candidate 'value))))


(cl-defmethod e-spotlight-input-update ((input e-spotlight-input) raw-str)
  (let* ((prev (e-spotlight-obj-get input 'raw))
         (separator (e-spotlight-obj-get input 'separator))
         (updated (not (string= prev raw-str)))
         (input-items (split-string raw-str separator t)))
    (eieio-oset input 'raw raw-str)
    (eieio-oset input 'prev prev)
    (eieio-oset input 'updated updated)
    (if (<= (length input-items) 1)
        (progn
          (eieio-oset input 'prefix nil)
          (eieio-oset input 'keyword (car input-items)))
      (eieio-oset input 'prefix (car input-items))
      (eieio-oset input 'keyword (string-join (cdr input-items) " ")))
    (unless (e-spotlight-obj-get input 'keyword)
      (eieio-oset input 'keyword "")))
  input)

(defun e-spotlight-copy (str)
  (when str
    (kill-new str)))

(provide 'e-spotlight-obj)
;;; e-spotlight-obj.el ends here
