;;; init.el --- Main configuration file
;;; Commentary:
;;; Emacs config by Andrey Orst
;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;;; Common settings:
(ignore-errors
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (fset 'menu-bar-open nil))
(set-face-attribute 'default nil :font "Source Code Pro-11")

(setq-default indent-tabs-mode nil
              scroll-step 1
              auto-window-vscroll nil
              cursor-type 'bar)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      ring-bell-function 'ignore
      custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)

;;; Packages
(defun ensure-installed (pkg)
  "Ensure that PKG is installed."
  (when (not (package-installed-p pkg))
    (package-install pkg)))

(ensure-installed 'use-package)
(require 'use-package)

(ensure-installed 'spacemacs-theme)
(load-theme 'spacemacs-dark t nil)
(set-face-attribute 'fringe nil :background nil)

(use-package diminish :ensure t)

(use-package highlight-indent-guides :ensure t
  :diminish highlight-indent-guides-mode
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?▏)
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package flx :ensure t)

(use-package company-flx :ensure t)

(use-package counsel :ensure t)

(use-package swiper :ensure t)

(use-package ivy :ensure t
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x M-f" . counsel-recentf)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("C-S-o" . counsel-rhythmbox))
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        ivy-count-format ""
        ivy-display-style nil
        ivy-minibuffer-faces nil)
  (ivy-mode 1)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package spaceline-config :ensure spaceline
  :config
  (spaceline-emacs-theme)
  :init
  (setq powerline-default-separator 'slant))

(use-package markdown-mode :ensure t)

(use-package multiple-cursors :ensure t
  :bind
  ("s-d" . mc/mark-next-like-this-word)
  ("s-D" . mc/mark-previous-like-this-word)
  :config
  (define-key mc/keymap (kbd "<return>") nil)
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
  (add-hook 'multiple-cursors-mode-enabled-hook
            '(lambda()
               (setq blink-matching-paren nil)))

  (add-hook 'multiple-cursors-mode-disabled-hook
            '(lambda()
               (setq blink-matching-paren t))))

(use-package projectile :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-svn-command "find . \\( -path '*/.svn*' -o -path '*/.git*' \\) -prune -o -type f -print0"
        projectile-generic-command "find . \\( -path '*/.svn*' -o -path '*/.git*' \\) -prune -o -type f -print0"
        projectile-git-command "find . \\( -path '*/.svn*' -o -path '*/.git*' \\) -prune -o -type f -print0"
        projectile-require-project-root nil
        projectile-enable-caching t
        projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode 1))

(use-package flycheck :ensure t)

(use-package lsp-mode :ensure t
  :config
  (setq lsp-highlight-symbol-at-point nil))

(use-package lsp-ui :ensure t)

(use-package company :ensure t
  :init
  (setq company-require-match 'never
        company-minimum-prefix-length 2
        company-frontends
        '(company-pseudo-tooltip-unless-just-one-frontend
          company-preview-frontend
          company-echo-metadata-frontend))
  :config
  (setq company-backends (remove 'company-clang company-backends)
        company-backends (remove 'company-xcode company-backends)
        company-backends (remove 'company-cmake company-backends)
        company-backends (remove 'company-elisp company-backends)
        company-backends (remove 'company-gtags company-backends))
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous))

(use-package company-lsp :ensure t
  :config
  (push '(company-lsp :with company-yasnippet) company-backends))

(use-package company-quickhelp :ensure t
  :config
  (company-quickhelp-mode))

(use-package yasnippet :ensure t
  :diminish yas-minor-mode
  :config
  (add-hook 'prog-mode-hook 'yas-minor-mode))

(use-package yasnippet-snippets :ensure t)

(use-package nlinum :ensure t
  :config
  (add-hook 'prog-mode-hook 'nlinum-mode)
  (set-face-attribute 'linum nil :background nil))

(use-package cquery :ensure t
  :init
  (setq cquery-executable "/usr/bin/cquery"
        cquery-cache-dir "~/.cache/cquery"
        cquery-sem-highlight-method 'font-lock)
  (add-hook 'c-mode-hook '(lambda() (lsp-cquery-enable))
            'c++-mode-hook '(lambda() (lsp-cquery-enable))))

(use-package sr-speedbar :ensure t
  :init
  (setq speedbar-use-images nil))

;;; Language settings

;;; C specific settings
(add-hook 'c-mode-common-hook
          '(lambda()
             (setq indent-tabs-mode t
                   c-basic-offset 4
                   tab-width 4)
             (lsp-cquery-enable)
             (lsp-ui-mode)
             (lsp-ui-doc-mode -1)
             (flycheck-mode)))

;;; eLisp specific settings
(add-hook 'emacs-lisp-mode-hook
          '(lambda()
             (flycheck-mode)
             (push '(company-elisp :with company-yasnippet) company-backends)))

(provide 'init)
;;; init.el ends here
