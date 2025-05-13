;;; early-init.el --- Early initialization -*- lexical-binding: t; no-byte-compile: t -*-

;; Author: Andrey Listopadov
;; Keywords: Emacs configuration
;; Homepage: https://gitlab.com/andreyorst/dotfiles.git

;;; Commentary:
;; Emacs 30+ early initialization configuration.

;;; Code:

(let ((original-gc-cons-threshold gc-cons-threshold))
  (setq
   gc-cons-threshold most-positive-fixnum
   inhibit-compacting-font-caches t
   message-log-max 16384
   package-enable-at-startup nil
   load-prefer-newer noninteractive)
  (add-hook
   'emacs-startup-hook
   (lambda nil
     (setq gc-cons-threshold original-gc-cons-threshold))))

(setq-default
 default-frame-alist '((width . 170)
                       (height . 52)
                       (tool-bar-lines . 0)
                       (bottom-divider-width . 0)
                       (right-divider-width . 1))
 initial-frame-alist default-frame-alist
 frame-inhibit-implied-resize t
 fringe-indicator-alist (assq-delete-all 'truncation fringe-indicator-alist))

(unless (or (daemonp) noninteractive)
  (let ((original-file-name-handler-alist file-name-handler-alist))
    (setq-default file-name-handler-alist nil)
    (add-hook
     'emacs-startup-hook
     (lambda nil
       (setq file-name-handler-alist
             (delete-dups (append file-name-handler-alist
                                  original-file-name-handler-alist)))
       (setq read-process-output-max
             (or (and (eq system-type 'gnu/linux)
                      (ignore-error permission-denied
                        (with-temp-buffer
                          (insert-file-contents-literally
                           "/proc/sys/fs/pipe-max-size")
                          (string-to-number
                           (buffer-substring-no-properties
                            (point-min) (point-max))))))
                 (* 4 1024 1024))))
     101))
  (when (fboundp #'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp #'scroll-bar-mode)
    (scroll-bar-mode -1)))

(when (featurep 'native-compile)
  (defvar native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors 'silent))

(defun edit-init-file ()
  "Edit `user-init-file'.
With prefix argument promtps to select a file from all Emacs Lisp
in `user-emacs-directory'."
  (interactive)
  (if current-prefix-arg
      (find-file
       (expand-file-name
        (completing-read
         "file"
         (directory-files user-emacs-directory nil "^[^.].*.el$"))
        user-emacs-directory))
    (find-file (expand-file-name "init.el" user-emacs-directory))))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))
(package-initialize)

(provide 'early-init)
;;; early-init.el ends here
