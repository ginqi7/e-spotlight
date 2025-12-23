;;; e-spotlight.el --- Gloabl Interactive Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
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

;;; Installation:
;; Manual:
;; Download the source code and put it wherever you like, e.g. into
;; ~/.emacs.d/e-spotlight/
;; ```
;; git clone git@github.com:ginqi7/e-spotlight.git
;; ```
;; Add the downloaded directory to the load path:
;; ```
;; (add-to-list 'load-path "~/.emacs.d/e-spotlight/")
;; (require 'e-spotlight)
;; ```
;;

;;; Code:

(provide 'e-spotlight-obj)
(require 'e-spotlight-frame)

(defvar e-spotlight--candidates nil "Candidates.")
(defvar e-spotlight--selected-index 0 "Selected index.")
(defvar e-spotlight--selected-overlay nil "Selected overlay.")
(defvar e-spotlight--last-app nil "Last front app.")

(defconst e-spotlight--components '(input candidates preview)
  "List of UI components.")

(defvar e-spotlight--input
  (e-spotlight-input
   :raw ""
   :type ""
   :link '("")
   :prev ""
   :updated nil)
  "input.")


(defvar e-spotlight--buffers
  (make-hash-table :test 'equal)
  "Buffers.")
(defvar e-spotlight--frames
  (make-hash-table :test 'equal)
  "Frames.")

(defvar e-spotlight-plugins nil)

(defvar e-spotlight--current-plugins nil)

(defun e-spotlight--extract (completions)
  "Extract completions, removing the length tail if present."
  ;; NOTE: completions 可能是以整数结尾的非正规列表
  ;; 使用 length, dolist butlast 等列表操作都可能会报错
  ;; 可以使用 (take (1- (safe-length completions) completions)) 去除最后一个元素
  (if (proper-list-p completions)
      completions
    (take (1- (safe-length completions)) completions)))


(defun e-spotlight--filter (str table)
  "Filter elements by STR in TABLE."
  (mapcar (lambda (key) (gethash key table))
          (e-spotlight--extract
           (completion-all-completions str (hash-table-keys table) nil nil))))


(defun e-spotlight--update-candidates ()
  "Update candidates."
  (setq e-spotlight--candidates
        (cl-mapcan (lambda (p) (e-spotlight-plugin-candidates p e-spotlight--input))
                   e-spotlight--current-plugins))
  (e-spotlight-update))


(defun e-spotlight--get-buffer (name)
  (gethash name e-spotlight--buffers))


(defun e-spotlight--init-input-buffer-watcher ()
  (when-let* ((buff (e-spotlight--get-buffer 'input)))    
    (with-current-buffer buff
      (when (featurep 'meow)
        (meow-insert))
      (add-hook 'after-change-functions
                (lambda (start end length)
                  (e-spotlight-update-input))
                nil t))))

(defun e-spotlight--get-input ()
  "Update candidates."
  (when-let ((buff (e-spotlight--get-buffer 'input)))    
    (with-current-buffer buff
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun e-spotlight--mark-selected-candidate ()
  "Mark selected candidate."
  (if e-spotlight--selected-overlay
      (delete-overlay e-spotlight--selected-overlay))
  (goto-char (point-min))
  (forward-line e-spotlight--selected-index)
  (setq e-spotlight--selected-overlay
        (make-overlay (line-beginning-position) (line-end-position)))
  (overlay-put
   e-spotlight--selected-overlay
   'face calendar-holiday-marker))

(defun e-spotlight--update-candidates-buffer ()
  "Refresh candidates buffer."
  (when-let* ((buff (e-spotlight--get-buffer 'candidates)))
    (with-current-buffer buff
      (erase-buffer)
      (dolist (candidate e-spotlight--candidates)
        (when (e-spotlight-obj-get candidate 'icon)
          (insert-image (e-spotlight-obj-get candidate 'icon)))
        (insert (eieio-oref candidate 'key))
        (insert "\n"))
      (e-spotlight--mark-selected-candidate))))

(defun e-spotlight--update-preview-buffer ()
  "Refresh preview buffer."
  (when-let* ((buff (e-spotlight--get-buffer 'preview))
              (selected-candidate (e-spotlight-selected-candidate))
              (preview (e-spotlight-obj-get selected-candidate 'preview))
              (arg (e-spotlight-obj-get selected-candidate 'value)))
    (with-current-buffer buff
      (erase-buffer)
      (if (stringp preview)
          (insert preview)
        (insert (funcall preview arg))))))

(defun e-spotlight--buffer-new (name)
  "Create buffer by NAME."
  (puthash name
           (get-buffer-create
            (format "*e-spotlight-%s*" name))
           e-spotlight--buffers))

(defun e-spotlight ()
  "Global Interactive Emacs."
  (interactive)
  (when (featurep 'macos-controller) (macc-get-actived-app))
  (setq e-spotlight--current-plugins e-spotlight-plugins)
  (e-spotlight-frame-init)
  (let ((input-frame
         (gethash 'input e-spotlight--frames)))
    (make-frame-visible input-frame)
    (e-spotlight--init-input-buffer-watcher)))

(defun e-spotlight-frame-init ()
  "Global Interactive Emacs Frame Init."
  (setq e-spotlight--selected-index 0)
  (dolist (comp e-spotlight--components)
    (e-spotlight--buffer-new comp)
    (e-spotlight--frame-new comp)))

(defun e-spotlight-update-input ()
  (e-spotlight-input-update e-spotlight--input (e-spotlight--get-input))
  (when (e-spotlight-obj-get e-spotlight--input 'updated)
    (e-spotlight--update-candidates)))

(defun e-spotlight-update (&rest _)
  "Update."
  (e-spotlight--update-candidates-buffer)
  (e-spotlight--update-candidates-frame)
  (e-spotlight--update-preview-buffer)
  (e-spotlight--update-preview-frame))

(defun e-spotlight-quit-back ()
  (interactive)
  (e-spotlight-quit)
  (when e-spotlight--last-app
    (do-applescript (format "tell application \"%s\" to activate" e-spotlight--last-app))
    (setq e-spotlight--last-app nil)))

(defun e-spotlight-quit ()
  "Delete all global interactive Emacs frames."
  (interactive)
  (maphash (lambda (_ frame) (delete-frame frame)) e-spotlight--frames)
  (maphash (lambda (_ buffer) (kill-buffer buffer)) e-spotlight--buffers)

  (clrhash e-spotlight--frames)
  (clrhash e-spotlight--buffers))

(defun e-spotlight-selected-candidate ()
  (nth e-spotlight--selected-index e-spotlight--candidates))

(defun e-spotlight-select-next (&optional arg)
  "Select next candidate. ARG -1 for previous."
  (interactive)
  (let ((delta (or arg 1))
        (len (length e-spotlight--candidates)))
    (setq e-spotlight--selected-index
          (mod (+ e-spotlight--selected-index delta) (max 1 len)))
    (e-spotlight-update)))

(defun e-spotlight-select-previous ()
  "Select previous candidate."
  (interactive)
  (e-spotlight-select-next -1))

(defun e-spotlight-show-plugins ()
  (interactive)
  (when-let* ((buff (e-spotlight--get-buffer 'input))
              (selected-candidate (e-spotlight-selected-candidate))
              (plugins (e-spotlight-obj-get selected-candidate 'plugins)))
    (setq e-spotlight--current-plugins plugins)
    (with-current-buffer buff
      (erase-buffer))))

(defun e-spotlight-run ()
  "Run selected candidate."
  (interactive)
  (when-let* ((selected-candidate
               (nth e-spotlight--selected-index
                    e-spotlight--candidates)))
    (e-spotlight-candidate-run selected-candidate)
    (e-spotlight-quit)))

(define-minor-mode e-spotlight-input-mode
  "Global interactive Emacs Input mode."
  :keymap (let ((map (make-sparse-keymap)))
            (keymap-set map "RET" #'e-spotlight-run)
            (keymap-set map "<TAB>" #'e-spotlight-show-plugins)
            (keymap-set map "C-g" #'e-spotlight-quit-back)
            (keymap-set map "C-p" #'e-spotlight-select-previous)
            (keymap-set map "C-n" #'e-spotlight-select-next)
            map))

(provide 'e-spotlight)
;;; e-spotlight.el ends here
