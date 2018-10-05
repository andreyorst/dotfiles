;; Common settings
(ignore-errors
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (fset 'menu-bar-open nil)
  (set-face-attribute 'default nil :font "Source Code Pro-10"))

(setq-default indent-tabs-mode nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)

(defun ensure-installed (pkg)
  "Ensure that package is installed"
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

(use-package highlight-indent-guides
  :ensure highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?▏
        highlight-indent-guides-responsive 'top)
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package helm
  :ensure helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-b" . helm-buffers-list)
         ("C-x M-f" . helm-recentf)))

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-emacs-theme)
  (spaceline-helm-mode)
  :init
  (setq powerline-default-separator 'wave))

(use-package iedit
  :ensure iedit
  :bind
  ("C-;" . iedit-mode))

(use-package markdown-mode
  :ensure markdown-mode)

;; C specific settings
(add-hook 'c-or-c++-mode
          '(lambda()
             (setq indent-tabs-mode t)
             (setq tab-width 4)
             (defvaralias 'c-basic-offset 'tab-width)
             (defvaralias 'cperl-indent-level 'tab-width)))

(use-package yasnippet-snippets
  :ensure yasnippet-snippets)

(use-package yasnippet
  :ensure yasnippet
  :config
  (add-hook 'prog-mode-hook 'yas-minor-mode))
