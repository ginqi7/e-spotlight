;;; e-spotlight-process.el ---                       -*- lexical-binding: t; -*-

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

(require 'e-spotlight-cache)

(defvar e-spotlight-process--queue nil)
(defvar e-spotlight-process--queue-running nil)

(defcustom e-spotlight-process-async-debug nil "")

;;; --- Queue Management ---
(defun e-spotlight-process-queue (name program &rest program-args)
  "Add a process to the queue. FIFO order."
  ;; 使用 nconc 添加到队尾，保证先进先出 (FIFO)
  (setq e-spotlight-process--queue
        (nconc e-spotlight-process--queue
               (list (list :name name :program program :args program-args))))
  (e-spotlight--process-queue-next))

(defun e-spotlight--process-queue-next ()
  "Run the next item in the queue if idle."
  (when (and e-spotlight-process--queue
             (not e-spotlight-process--queue-running))
    (let* ((task (pop e-spotlight-process--queue))
           (name (plist-get task :name))
           (prog (plist-get task :program))
           (args (plist-get task :args)))
      (setq e-spotlight-process--queue-running t)
      ;; 这里的 callback 是队列内部用来触发下一个任务的
      (apply #'e-spotlight-process-async-start
             name prog
             (lambda (_output)
               (setq e-spotlight-process--queue-running nil)
               (e-spotlight--process-queue-next))
             (lambda (_err)
               (setq e-spotlight-process--queue-running nil)
               (e-spotlight--process-queue-next))
             args))))

;;; --- Async Process Runner ---
(defun e-spotlight-process-async-start (name program callback err-callback &rest program-args)
  (let* ((buf (generate-new-buffer (format "*%s*" name)))
         (buf-err (generate-new-buffer (format "*%s:err*" name)))
         (proc (let ((process-connection-type nil))
                 (make-process
                  :name name
                  :buffer buf
                  :stderr buf-err
                  :command (cons program program-args)
                  :noquery t
                  :sentinel (lambda (proc event)
                              (e-spotlight--process-sentinel proc event callback err-callback))))))))

(defun e-spotlight--process-sentinel (proc event callback err-callback)
  "Handle process finish or death."
  (when (memq (process-status proc) '(exit signal))
    (let ((buf (process-buffer proc))
          (buf-err (process-get proc 'stderr)))
      (unwind-protect
          (progn
            ;; 处理标准输出
            (when (and callback (buffer-live-p buf))
              (with-current-buffer buf
                (funcall callback (buffer-string))))
            ;; 处理错误输出
            (when (and err-callback (buffer-live-p buf-err))
              (with-current-buffer buf-err
                (let ((err-content (buffer-string)))
                  (unless (string-empty-p err-content)
                    (funcall err-callback err-content))))))
        ;; 清理资源 (除非开启调试模式)
        (unless e-spotlight-process-async-debug
          (when (buffer-live-p buf) (kill-buffer buf))
          (when (buffer-live-p buf-err) (kill-buffer buf-err)))))))


(defun e-spotlight-process-async-run-shell (name shell-command ttl callback)
  (if-let ((value (e-spotlight-cache-get "process" name)))
      (funcall callback value)
    (e-spotlight-process-async-start
     name
     "sh"
     (lambda (value)
       (e-spotlight-cache-put "process" name value ttl)
       value)
     nil
     "-c"
     shell-command)
    nil))


(provide 'e-spotlight-process)
;;; e-spotlight-process.el ends here

