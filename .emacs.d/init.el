;;; init.el --- Emacs main configuration file
;;; Commentary:
;;; Emacs config by Andrey Orst
;;; Main config is located in .emacs.d/config.org
;;; This file contains some speed hacks taken from Doom Emacs
;;; Code:

;; Avoid garbage collection during startup. The GC eats up quite a bit of
;; time, easily doubling startup time. The trick is to turn up the memory
;; threshold to prevent it from running
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Unset file-name-handler-alist
(defvar my--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Package.el initialization is expensive, so disable it too
(setq package-enable-at-startup nil ;; don't auto-initialize!
      ;; don't add that `custom-set-variables' block to my init!
      package--init-file-ensured t)

;; Configuration with org-babel
(require 'org)
(org-babel-load-file "~/.emacs.d/config.org")

;; reset garbage collection
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)

;; restore file-name-handler-alist
(setq file-name-handler-alist my--file-name-handler-alist)

(provide 'init)
;;; init.el ends here
