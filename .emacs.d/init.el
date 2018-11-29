;;; init.el --- Emacs main configuration file
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
(setq inhibit-splash-screen t)

(global-display-line-numbers-mode)

(ignore-errors
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (fset 'menu-bar-open nil))

(set-face-attribute 'default nil :font "Source Code Pro-10")

(setq-default indent-tabs-mode nil
              scroll-step 1
              auto-window-vscroll nil
              cursor-type 'bar)

;; disable bell
(setq ring-bell-function 'ignore)

;; move custom settings to separate custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)

;; store backups in different place and disable lockfiles
(setq backup-by-copying t
      create-lockfiles nil
      backup-directory-alist '(("." . "~/.cache/emacs-backups"))
      auto-save-file-name-transforms '((".*" "~/.cache/emacs-backups" t)))

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

(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ;; should be included.
            pretty-parens  ;; different paren styles for different modes.
            paredit        ;; Introduce some paredit commands.
            smart-tab      ;; C-b & C-f jump positions and smart shift with tab & S-tab.
            smart-yank))   ;; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    (add-hook 'scheme-mode-hook #'parinfer-mode)
    (add-hook 'lisp-mode-hook #'parinfer-mode)))

(use-package diminish :ensure t)

(use-package highlight-indent-guides :ensure t
  :diminish highlight-indent-guides-mode
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?▏)
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package flx :ensure t)

(use-package ivy :ensure t
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
  :diminish ivy-mode
  :config
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))
        ivy-count-format ""
        ivy-display-style nil
        ivy-minibuffer-faces nil)
  (ivy-mode 1)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package counsel :ensure t)

(use-package swiper :ensure t)

(use-package markdown-mode :ensure t)

(use-package flycheck :ensure t)

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
        company-backends (remove 'company-gtags company-backends))
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous))

(use-package company-flx :ensure t)

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

;;; Language settings

;;; eLisp specific settings
(add-hook 'emacs-lisp-mode-hook
          '(lambda()
             (flycheck-mode)
             (push '(company-elisp :with company-yasnippet) company-backends)))

(provide 'init)
;;; init.el ends here
