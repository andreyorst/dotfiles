;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Author: Andrey Listopadov
;; Keywords: Emacs configuration
;; Homepage: https://gitlab.com/andreyorst/dotfiles

;;; Commentary:
;; Emacs early initialization configuration.

;;; Code:

(setq
 gc-cons-threshold most-positive-fixnum
 read-process-output-max (* 1024 1024 4) ; 4mb
 inhibit-compacting-font-caches t
 message-log-max 16384
 package-enable-at-startup nil
 load-prefer-newer noninteractive)

(setq-default
 initial-frame-alist '((width . 170)
                       (height . 56)
                       (tool-bar-lines . 0)
                       (bottom-divider-width . 0)
                       (right-divider-width . 1))
 default-frame-alist initial-frame-alist
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
  (defvar native-comp-deferred-compilation)
  (setq native-comp-deferred-compilation t)
  (defvar native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors nil))

;;; Straight

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

(provide 'early-init)
;;; early-init.el ends here
