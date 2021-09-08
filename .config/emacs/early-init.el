;;; early-init.el --- Early initialization -*- lexical-binding: t; buffer-read-only: t; no-byte-compile: t -*-

;; Author: Andrey Listopadov
;; Keywords: literate programming, Emacs configuration
;; Homepage: https://gitlab.com/andreyorst/dotfiles/-/tree/master/.config/emacs

;;; Commentary:
;; Emacs config.
;; This file was automatically generated by `org-babel-tangle'.
;; Do not change this file.  Main config is located in README.org at `user-emacs-directory'

;;; Code:

(defvar aorst--gc-cons-threshold gc-cons-threshold)
(defvar aorst--gc-cons-percentage gc-cons-percentage)
(defvar aorst--file-name-handler-alist file-name-handler-alist)

(setq-default gc-cons-threshold 402653184
              gc-cons-percentage 0.6
              file-name-handler-alist nil)

(defun aorst/restore-defaults-after-init ()
  "Restore default values after initialization."
  (setq-default gc-cons-threshold aorst--gc-cons-threshold
                gc-cons-percentage aorst--gc-cons-percentage
                file-name-handler-alist aorst--file-name-handler-alist))

(add-hook 'after-init-hook #'aorst/restore-defaults-after-init)

(setq read-process-output-max (* 1024 1024 4) ; 4mb
      inhibit-compacting-font-caches t
      message-log-max 16384)

(when (featurep 'native-compile)
  (setq native-comp-deferred-compilation t)
  (setq native-comp-async-report-warnings-errors nil))

(setq-default initial-frame-alist '((width . 170)
                                    (height . 56)
                                    (tool-bar-lines . 0)
                                    (bottom-divider-width . 0)
                                    (right-divider-width . 1))
              default-frame-alist initial-frame-alist
              frame-inhibit-implied-resize t
              x-gtk-resize-child-frames 'resize-mode
              fringe-indicator-alist (assq-delete-all 'truncation fringe-indicator-alist))

(defvar straight-process-buffer)
(setq-default straight-process-buffer " *straight-process*")

(defvar straight-build-dir)
(setq straight-build-dir (format "build-%s" emacs-version))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

(setq load-prefer-newer noninteractive)

(provide 'early-init)
;;; early-init.el ends here
