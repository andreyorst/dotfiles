;;; init.el --- Emacs main configuration file
;;; Commentary:
;;; Emacs config by Andrey Orst
;;; Code:

;; speed hacks from Doom Emacs

;; Avoid garbage collection during startup. The GC eats up quite a bit of
;; time, easily doubling startup time. The trick is to turn up the memory
;; threshold to prevent it from running

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; unset file-name-handler-alist

(defvar my--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Package.el initialization is expensive, so disable it too

;; (setq package-enable-at-startup nil ; don't auto-initialize!
;;       ;; don't add that `custom-set-variables' block to my initl!
;;       package--init-file-ensured t)

;; Configuration with org-babel

(require 'org)
(org-babel-load-file "~/.emacs.d/config.org")

;; reset garbace collection

(add-hook 'emacs-startup-hook
          (setq gc-cons-threshold 16777216
                gc-cons-percentage 0.1))

;; restore file-name-handler-alist

(add-hook 'emacs-startup-hook
          (setq file-name-handler-alist my--file-name-handler-alist))

(provide 'init)
;;; init.el ends here
