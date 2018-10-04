;; Common settings
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(fset 'menu-bar-open nil)
(set-face-attribute 'default nil :font "Source Code Pro-10")
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)

;; Install a package if it wasn't installed before
(defun ensure-installed (pkg)
  (when (not (package-installed-p pkg))
    (package-install pkg)))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(ensure-installed 'use-package)
(require 'use-package)

(ensure-installed 'spacemacs-theme)
(load-theme 'spacemacs-dark t nil)

(ensure-installed 'highlight-indent-guides)
(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character
	highlight-indent-guides-character ?▏
	highlight-indent-guides-responsive 'top)
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(ensure-installed 'helm)
(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-buffers-list)
         ("C-x M-f" . helm-recentf)))

(ensure-installed 'spaceline)
(use-package spaceline-config
  :config
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode))

(ensure-installed 'iedit)
(use-package iedit
  :bind
  ("C-;" . iedit-mode))

(ensure-installed 'markdown-mode)
(use-package markdown-mode)

;; C specific settings
(add-hook 'c-or-c++-mode
	  (setq indent-tabs-mode t)
	  (setq c-basic-offset 4)
	  (setq-default tab-width 4))

(ensure-installed 'yasnippet-snippets)
(use-package yasnippet-snippets)
(ensure-installed 'yasnippet)
(use-package yasnippet
  :config
  (add-hook 'prog-mode-hook 'yas-minor-mode))
