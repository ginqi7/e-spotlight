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
   (raw :initarg :raw)
   (type :initarg :type)
   (link :initarg :link)
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
  (let* ((raw-str (eieio-oref
                   e-spotlight--input
                   'raw)))
    (let ((fetch (e-spotlight-obj-get plugin 'fetch))
          (command (if (stringp (e-spotlight-obj-get plugin 'command))
                       (e-spotlight-obj-get plugin 'command)
                     (funcall (e-spotlight-obj-get plugin 'command) raw-str)))
          (ttl (e-spotlight-obj-get plugin 'ttl))
          (parse (e-spotlight-obj-get plugin 'parse))
          (filter (e-spotlight-obj-get plugin 'filter)))
      (funcall fetch
               (symbol-name (class-name (object-class plugin)))
               command
               ttl
               (lambda (output)
                 (when parse
                   (setq output (funcall parse output)))
                 (when filter
                   (setq output (funcall filter raw-str output)))
                 (setq e-spotlight--candidates (append e-spotlight--candidates output))
                 (e-spotlight-update))))))


(cl-defmethod e-spotlight-candidate-run ((candidate e-spotlight-candidate))
  (when-let* ((action (e-spotlight-obj-get candidate 'action)))
    (funcall action (eieio-oref candidate 'value))))


(cl-defmethod e-spotlight-input-update ((input e-spotlight-input) raw-str)
  (let* ((prev (eieio-oref input 'raw))
         (updated (not (string= prev raw-str))))
    (eieio-oset input 'raw raw-str)
    (eieio-oset input 'prev prev)
    (eieio-oset input 'updated updated)
    (eieio-oset input 'link nil)
    input))

(provide 'e-spotlight-obj)
;;; e-spotlight-obj.el ends here
