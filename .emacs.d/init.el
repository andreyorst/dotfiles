(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; disable distracting gui features:
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; default font:
(set-face-attribute 'default nil :font "Liberation Mono-10")
;; (set-face-attribute 'default t :font "Liberation Mono-10")
