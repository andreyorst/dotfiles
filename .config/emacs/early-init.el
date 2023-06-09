;;; early-init.el --- Early initialization -*- lexical-binding: t; no-byte-compile: t -*-

;; Author: Andrey Listopadov
;; Keywords: Emacs configuration
;; Homepage: https://gitlab.com/andreyorst/dotfiles.git

;;; Commentary:
;; Emacs 29+ early initialization configuration.

;;; Code:

(setq
 gc-cons-threshold most-positive-fixnum
 read-process-output-max (* 1024 1024 4) ; 4mb
 inhibit-compacting-font-caches t
 message-log-max 16384
 package-enable-at-startup nil
 load-prefer-newer noninteractive)

(setq-default
 default-frame-alist '((width . 170)
                       (height . 56)
                       (tool-bar-lines . 0)
                       (bottom-divider-width . 0)
                       (right-divider-width . 1))
 initial-frame-alist default-frame-alist
 frame-inhibit-implied-resize t
 x-gtk-resize-child-frames 'resize-mode
 fringe-indicator-alist (assq-delete-all 'truncation fringe-indicator-alist))

(unless (or (daemonp) noninteractive)
  (let ((restore-file-name-handler-alist file-name-handler-alist))
    (setq-default file-name-handler-alist nil)
    (defun restore-file-handler-alist ()
      (setq file-name-handler-alist
            (delete-dups (append file-name-handler-alist
                                 restore-file-name-handler-alist)))))

  (add-hook 'emacs-startup-hook #'restore-file-handler-alist 101)

  (when (fboundp #'tool-bar-mode)
    (tool-bar-mode -1))

  (when (fboundp #'scroll-bar-mode)
    (scroll-bar-mode -1)))

(when (featurep 'native-compile)
  (defvar inhibit-automatic-native-compilation)
  (setq inhibit-automatic-native-compilation nil)
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
(package-initialize)

(provide 'early-init)
;;; early-init.el ends here
