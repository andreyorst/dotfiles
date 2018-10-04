;; Common settings
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(fset 'menu-bar-open nil)
(set-face-attribute 'default nil :font "Source Code Pro-10")

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(require 'use-package)

(load-theme 'spacemacs-dark t nil)

(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?▏)
  (setq highlight-indent-guides-responsive 'top)
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-buffers-list)
         ("C-x M-f" . helm-recentf)))

(use-package spaceline-config
  :config
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode))

(use-package iedit
  :bind
  ("C-;" . iedit-mode))

(use-package markdown-mode)

;; C specific settings
(setq indent-tabs-mode t)
(setq c-basic-offset 4)
(setq-default tab-width 4)

(use-package yasnippet-snippets)
(use-package yasnippet
  :config
  (add-hook 'prog-mode-hook 'yas-minor-mode))
