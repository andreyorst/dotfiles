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
(fset 'menu-bar-open nil)

;; default font:

(set-face-attribute 'default nil :font "Source Code Pro-10")
;; theme
(load-theme 'spacemacs-dark t nil)
;; (load-theme 'gruvbox-dark-soft t nil)

(custom-set-variables
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "06239857eba54280977d62190690bef6d58f9653465c71b0fe279c560fdcac80" default)))
 '(package-selected-packages (quote (spacemacs-theme helm gruvbox-theme monokai-theme))))
(custom-set-faces
 )
