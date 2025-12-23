;;; e-spotlight-frame.el --- Configration for global interactive emacs.  -*- lexical-binding: t; -*-




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
;; ~/.emacs.d/jin-emacs/
;; ```
;; git clone git@github.com:ginqi7/jin-emacs.git
;; ```
;; Add the downloaded directory to the load path:
;; ```
;; (add-to-list 'load-path "~/.emacs.d/jin-emacs/")
;; (require 'init-new-e-spotlight)
;; ```
;;

;;; Code:

(defvar e-spotlight-frame--input-font
  (font-spec :family
             (face-attribute 'default :family)
             :size
             25)
  "Input Font.")

(defun e-spotlight--update-candidates-frame ()
  "Refresh candidates frame."
  (let* ((input-frame
          (gethash 'input e-spotlight--frames))
         (candidates-frame
          (gethash 'candidates e-spotlight--frames)))
    
    (when (and input-frame candidates-frame)
      (set-frame-parameter candidates-frame 'parent-frame input-frame)
      (set-frame-position candidates-frame 0 (+ 5 (frame-char-height input-frame)))
      (set-frame-height candidates-frame (min 30 (floor (* 2 (length e-spotlight--candidates)))))
      (set-frame-width candidates-frame
                       (+ 50 (e-spotlight-frame--candidates-width e-spotlight--candidates))
                       nil t)
      ;; (set-frame-width candidates-frame
      ;;                  (e-spotlight-frame-max-pixel-width candidates-frame)
      ;;                  nil t)
      (make-frame-visible candidates-frame)
      (select-frame-set-input-focus input-frame)))
  (e-spotlight--update-preview-frame))

(defun e-spotlight--update-preview-frame ()
  (let* ((input-frame
          (gethash 'input e-spotlight--frames))
         (candidates-frame
          (gethash 'candidates e-spotlight--frames))
         (preview-frame
          (gethash 'preview e-spotlight--frames)))
    (when (and input-frame candidates-frame preview-frame)
      (set-frame-parameter preview-frame 'parent-frame input-frame)
      (set-frame-position preview-frame (+ 2 (frame-pixel-width candidates-frame)) (+ 5 (frame-char-height input-frame)))
      (set-frame-height preview-frame 300 nil t)
      (set-frame-width preview-frame 500 nil t)
      (when (gethash 'preview e-spotlight--buffers)
        (with-current-buffer (gethash 'preview e-spotlight--buffers)
          (if (string-empty-p (buffer-string))
              (make-frame-invisible preview-frame)
            (make-frame-visible preview-frame))))
      (select-frame-set-input-focus input-frame))))

(defun e-spotlight-frame--candidates-width (candidates)
  (let ((max-width 0)
        (display-str)
        (icon)
        (display-str-width)
        (icon-width))
    (dolist (candidate candidates)
      (setq display-str (eieio-oref candidate 'key))
      (setq icon (and (slot-boundp candidate 'icon) (eieio-oref candidate 'icon)))
      (setq display-str-width (string-pixel-width display-str))
      (setq icon-width (if icon
                           (ceiling (car (image-size icon)))
                         0))
      (setq max-width (max max-width (+ display-str-width icon-width))))
    (min 500 max-width)))

(defun e-spotlight--buffer-new (name)
  "Create buffer by NAME."
  (puthash name
           (get-buffer-create
            (format "*e-spotlight-%s*" name))
           e-spotlight--buffers))

(defun e-spotlight-frame-init ()
  "Global Interactive Emacs Frame Init."
  (setq e-spotlight--selected-index 0)
  (e-spotlight--buffer-new 'input)
  (e-spotlight--buffer-new 'candidates)
  (e-spotlight--buffer-new 'preview)
  (e-spotlight--buffer-new 'actions)

  (e-spotlight--frame-new 'input)
  (e-spotlight--frame-new 'candidates)
  (e-spotlight--frame-new 'preview)
  (e-spotlight--frame-new 'actions))

(defun e-spotlight-frame--screen-geometry ()
  (alist-get 'geometry
             (car (display-monitor-attributes-list))))

(defun e-spotlight-frame--screen-height ()
  (nth 3 (e-spotlight-frame--screen-geometry)))

(defun e-spotlight-frame--screen-width ()
  (nth 2 (e-spotlight-frame--screen-geometry)))

(defun e-spotlight-frame--width()
  (floor (* 0.3 (e-spotlight-frame--screen-width))))

(defun e-spotlight-frame--left ()
  (floor (* 0.5 (- (e-spotlight-frame--screen-width)
                   (e-spotlight-frame--width)))))

(defun e-spotlight-frame--top ()
  (floor (* 0.2 (e-spotlight-frame--screen-height))))

(defun e-spotlight--frame-new (name)
  "Create a frame by NAME."
  (unless (gethash name e-spotlight--frames)
    (let ((frame
           (make-frame
            `((name .
                    ,(format "e-spotlight-frame-%s" name))
              (no-accept-focus . nil)
              (no-focus-on-map . nil)
              (min-width . 0)
              (min-height . 0)
              (left . ,(e-spotlight-frame--left))
              (top . ,(e-spotlight-frame--top))
              (width . (text-pixels . ,(e-spotlight-frame--width)))
              (height . 0)
              (text-height . 10)
              (native-height . 10)
              (border-width . 0)
              (left-fringe . 10)
              (right-fringe . 10)
              (vertical-scroll-bars . nil)
              (horizontal-scroll-bars . nil)
              (menu-bar-lines . 0)
              (tool-bar-lines . 0)
              (tab-bar-lines . 0)
              (no-other-frame . t)
              (no-other-window . t)
              (no-delete-other-windows . t)
              (unsplittable . t)
              (undecorated . t)
              (cursor-type . nil)
              (visibility . nil)
              (minibuffer . nil)
              (no-special-glyphs . t)
              (desktop-dont-save . t)
              (mode-line-format . nil)))))
      (set-frame-font e-spotlight-frame--input-font nil (list frame))
      (switch-to-buffer
       (gethash name e-spotlight--buffers))
      (e-spotlight-input-mode)
      (erase-buffer)
      (puthash name frame e-spotlight--frames)
      frame)))

(provide 'e-spotlight-frame)
;;; e-spotlight-frame.el ends here
