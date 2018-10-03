(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(require 'use-package)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(fset 'menu-bar-open nil)

(set-face-attribute 'default nil :font "Source Code Pro-10")
(load-theme 'spacemacs-dark t nil)

(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?▏)
  (setq highlight-indent-guides-responsive 'top)
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;; C settings
(setq indent-tabs-mode t)
(setq c-basic-offset 4)
(setq-default tab-width 4)

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("M-<f5>" . helm-find-files)
         ([f10] . helm-buffers-list)
         ([S-f10] . helm-recentf)))

(use-package spaceline-config
  :init
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode))
