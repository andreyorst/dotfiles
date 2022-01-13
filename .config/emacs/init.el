;;; init.el --- Main configuration file -*- lexical-binding: t; -*-

;; Author: Andrey Listopadov
;; Keywords: Emacs configuration
;; Homepage: https://gitlab.com/andreyorst/dotfiles.git

;;; Commentary:
;; Emacs config.

;;; Code:

;;; Early init support for older Emacs

(unless (featurep 'early-init)
  (load (expand-file-name "early-init" user-emacs-directory)))

;;; use-package and straight.el

(require 'straight)
(straight-use-package 'use-package)
(require 'use-package)

(use-package straight
  :custom
  (straight-use-package-by-default t))

;;; Local config and personal functions

(use-package local-config
  :straight nil
  :preface
  (defgroup local-config ()
    "Customization group for local settings."
    :prefix "local-config-"
    :group 'emacs)
  (defcustom local-config-title-show-bufname t
    "Whether to include bufname in the title bar.

Bufname is not necessary on GNOME but may be useful in other DEs."
    :type 'boolean
    :tag "Title bufname"
    :group 'local-config)
  (defcustom local-config-dark-theme 'modus-vivendi
    "Dark theme to use."
    :tag "Dark theme"
    :type 'symbol
    :group 'local-config)
  (defcustom local-config-light-theme 'modus-operandi
    "Light theme to use."
    :tag "Light theme"
    :type 'symbol
    :group 'local-config)
  (defvar local-config-line-pixel-height (line-pixel-height)
    "Line height in pixels.

Used in various places to avoid getting wrong line height when
`text-scale-mode' is active.")
  (provide 'local-config))

(use-package functions
  :straight nil
  :preface
  (defun split-pararagraph-into-lines ()
    "Split the current paragraph into lines with one sentence each."
    (interactive)
    (save-excursion
      (let ((fill-column most-positive-fixnum))
        (fill-paragraph))
      (let ((auto-fill-p auto-fill-function)
            (end (progn (end-of-line) (backward-sentence) (point))))
        (back-to-indentation)
        (unless (= (point) end)
          (auto-fill-mode -1)
          (while (< (point) end)
            (forward-sentence)
            (delete-horizontal-space)
            (newline-and-indent))
          (deactivate-mark)
          (when auto-fill-p
            (auto-fill-mode t))
          (when (looking-at "^$")
            (backward-delete-char 1))))))
  (defun indirect-narrow-to-defun ()
    (interactive)
    (clone-indirect-buffer (buffer-name) t t)
    (narrow-to-defun))
  (defun indirect-narrow-to-region ()
    (interactive)
    (let ((beg (mark))
          (end (point)))
      (clone-indirect-buffer (buffer-name) t t)
      (narrow-to-region beg end)))
  (defmacro comment (&rest _)
    nil)
  (provide 'functions))

(use-package kmacro
  :straight nil
  :config
  (defun block-undo (fn &rest args)
    (let ((marker (prepare-change-group)))
      (unwind-protect (apply fn args)
        (undo-amalgamate-change-group marker))))
  (dolist (f '(kmacro-call-macro
               kmacro-exec-ring-item
               apply-macro-to-region-lines))
    (advice-add f :around #'block-undo)))

(use-package common-lisp-modes-mode
  :straight nil
  :bind ( :map common-lisp-modes-mode-map
          ("M-q" . common-lisp-modes-indent-or-fill-sexp))
  :preface
  (define-minor-mode common-lisp-modes-mode
    "Mode for enabling all modes that are common for lisps.

For reference, this is not a common-lisp modes mode, but a common
lisp-modes mode.

\\<common-lisp-modes-mode-map>"
    :lighter " clmm"
    :keymap (make-sparse-keymap))
  (defun common-lisp-modes-indent-or-fill-sexp ()
    "Indent s-expression or fill string/comment."
    (interactive)
    (let ((ppss (syntax-ppss)))
      (if (or (nth 3 ppss)
              (nth 4 ppss))
          (fill-paragraph)
        (save-excursion
          (mark-sexp)
          (indent-region (point) (mark))))))
  (provide 'common-lisp-modes-mode))

(use-package region-bindings
  :straight nil
  :bind ( :map region-bindings-mode-map
          ("q" . region-bindings-mode-off))
  :preface
  (define-minor-mode region-bindings-mode
    "Minor mode for mapping commands while region is active.

\\<region-bindings-mode-map>"
    :lighter " rbm"
    :group 'convenience
    :keymap (make-sparse-keymap))
  (defun region-bindings-mode--toggle (_ val _ _)
    (region-bindings-mode (if val 1 -1)))
  (defun region-bindings-mode-off ()
    "Turn off bindings while keeping the region active."
    (interactive)
    (region-bindings-mode -1))
  (defun region-bindings-mode-enable ()
    "Enable region bindings mode."
    (interactive)
    (add-variable-watcher 'transient-mark-mode #'region-bindings-mode--toggle))
  (defun region-bindings-mode-disable ()
    "Disable region bindings mode."
    (interactive)
    (remove-variable-watcher 'transient-mark-mode #'region-bindings-mode--toggle)
    (region-bindings-mode -1))
  (provide 'region-bindings)
  :init
  (region-bindings-mode-enable))

;;; Inbuilt stuff

(use-package defaults
  :straight nil
  :preface
  (setq-default
   indent-tabs-mode nil
   truncate-lines t
   bidi-paragraph-direction 'left-to-right
   frame-title-format
   '(:eval (if (or (eq (string-match "[ *]" (buffer-name)) 0)
                   (not local-config-title-show-bufname))
               "Emacs"
             (if (buffer-modified-p)
                 "%b* — Emacs"
               "%b — Emacs")))
   auto-window-vscroll nil
   mouse-highlight nil
   hscroll-step 1
   hscroll-margin 1
   scroll-margin 0
   scroll-preserve-screen-position nil
   scroll-conservatively 101)
  (when (window-system)
    (setq-default
     x-gtk-use-system-tooltips nil
     cursor-type 'box
     cursor-in-non-selected-windows nil))
  (setq
   ring-bell-function 'ignore
   mode-line-percent-position nil
   enable-recursive-minibuffers t)
  (when (version<= "27.1" emacs-version)
    (setq bidi-inhibit-bpa t))
  (provide 'defaults))

(use-package font
  :straight nil
  :preface
  (defun font-installed-p (font-name)
    "Check if a font with FONT-NAME is available."
    (find-font (font-spec :name font-name)))
  (provide 'font)
  :config
  (cond ((font-installed-p "JetBrainsMono")
         (set-face-attribute 'default nil :font "JetBrainsMono"))
        ((font-installed-p "Source Code Pro")
         (set-face-attribute 'default nil :font "Source Code Pro")))
  (when (font-installed-p "DejaVu Sans")
    (set-face-attribute 'variable-pitch nil :font "DejaVu Sans")))

(use-package cus-edit
  :straight nil
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :init
  (load custom-file :noerror))

(use-package novice
  :straight nil
  :init
  (defvar disabled-commands (expand-file-name "disabled.el" user-emacs-directory)
    "File to store disabled commands, that were enabled permanently.")
  (define-advice enable-command (:around (foo command))
    (let ((user-init-file disabled-commands))
      (funcall foo command)))
  (load disabled-commands :noerror))

(use-package startup
  :straight nil
  :no-require t
  :custom
  (user-mail-address "andreyorst@gmail.com")
  (user-full-name "Andrey Listopadov"))

(use-package files
  :straight nil
  :custom
  (backup-by-copying t)
  (create-lockfiles nil)
  (backup-directory-alist
   `(("." . ,(expand-file-name ".cache/backups" user-emacs-directory))))
  (auto-save-file-name-transforms
   `((".*" ,(expand-file-name ".cache/auto-save/" user-emacs-directory) t)))
  (auto-save-no-message t)
  (auto-save-interval 100)
  :config
  (let ((auto-save-dir (expand-file-name ".cache/auto-save/" user-emacs-directory)))
    (unless (file-exists-p auto-save-dir)
      (make-directory auto-save-dir t))))

(use-package subr
  :straight nil
  :no-require t
  :init
  (fset 'yes-or-no-p 'y-or-n-p))

(use-package mwheel
  :straight nil
  :bind (("S-<down-mouse-1>" . nil)
         ("S-<mouse-3>" . nil)
         ("<mouse-4>" . mwheel-scroll)
         ("<mouse-5>" . mwheel-scroll))
  :custom
  (mouse-wheel-flip-direction (not (featurep 'pgtk)))
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-progressive-speed nil)
  :init
  (global-set-key (kbd "<mouse-3>") menu-bar-edit-menu)
  (defun truncated-lines-p ()
    "Non-nil if any line is longer than `window-width' + `window-hscroll'.

Returns t if any line exceeds the right border of the window.
Used for stopping scroll from going beyond the longest line.
Based on `so-long-detected-long-line-p'."
    (save-excursion
      (goto-char (point-min))
      (let* ((window-width
              ;; this computes a more accurate width rather than `window-width', and respects
              ;; `text-scale-mode' font width.
              (/ (window-body-width nil t) (window-font-width)))
             (hscroll-offset
              ;; `window-hscroll' returns columns that are not affected by
              ;; `text-scale-mode'.  Because of that, we have to recompute the correct
              ;; `window-hscroll' by multiplying it with a non-scaled value and
              ;; dividing it with a scaled width value, rounding it to the upper
              ;; boundary.  Since there's no way to get unscaled value, we have to get
              ;; a width of a face that is not scaled by `text-scale-mode', such as
              ;; `window-divider' face.
              (ceiling (/ (* (window-hscroll) (window-font-width nil 'window-divider))
                          (float (window-font-width)))))
             (line-number-width
              ;; compensate line numbers width
              (if (bound-and-true-p display-line-numbers-mode)
                  (- display-line-numbers-width)
                0))
             ;; subtracting 2 for extra space in case some calculations were imprecise
             (threshold (+ window-width hscroll-offset line-number-width -2)))
        (catch 'excessive
          (while (not (eobp))
            (let ((start (point)))
              (save-restriction
                (narrow-to-region start (min (+ start 1 threshold)
                                             (point-max)))
                (forward-line 1))
              (unless (or (bolp)
                          (and (eobp) (<= (- (point) start)
                                          threshold)))
                (throw 'excessive t))))))))
  (define-advice scroll-left (:around (foo &optional arg set-minimum))
    (when (and truncate-lines
               (not (memq major-mode '(vterm-mode term-mode)))
               (truncated-lines-p))
      (funcall foo arg set-minimum)))
  (unless (display-graphic-p)
    (xterm-mouse-mode t)))

(use-package savehist
  :straight nil
  :config
  (savehist-mode 1))

(use-package mule-cmds
  :straight nil
  :no-require t
  :custom
  (default-input-method 'russian-computer)
  :init
  (prefer-coding-system 'utf-8))

(use-package select
  :straight nil
  :when (display-graphic-p)
  :custom
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package simple
  :straight nil
  :bind (("M-z" . zap-up-to-char)
         ("M-S-z" . zap-to-char)
         ("C-x k" . kill-this-buffer)
         ("C-h C-f" . describe-face))
  :hook ((before-save . delete-trailing-whitespace)
         (overwrite-mode . overwrite-set-cursor-shape))
  :custom
  (yank-excluded-properties t)
  (blink-matching-delay 0)
  (blink-matching-paren t)
  (copy-region-blink-delay 0)
  :init
  (column-number-mode 1)
  (line-number-mode 1)
  (transient-mark-mode -1)
  (defun overwrite-set-cursor-shape ()
    (when (display-graphic-p)
      (setq cursor-type (if overwrite-mode 'hollow 'box))))
  (define-advice keyboard-quit (:around (quit))
    "Quit the current context.

When there is an active minibuffer and we are not inside it close
it.  When we are inside the minibuffer use the regular
`minibuffer-keyboard-quit' which quits any active region before
exiting.  When there is no minibuffer `keyboard-quit' unless we
are defining or executing a macro."
    (if (active-minibuffer-window)
        (if (minibufferp)
            (minibuffer-keyboard-quit)
          (abort-recursive-edit))
      (unless (or defining-kbd-macro
                  executing-kbd-macro)
        (funcall-interactively quit)))))

(use-package delsel
  :straight nil
  :init
  (delete-selection-mode t))

(use-package minibuffer
  :straight nil
  :bind ( :map minibuffer-inactive-mode-map
          ("<mouse-1>" . ignore))
  :custom
  (completion-styles '(partial-completion basic))
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :custom-face
  (completions-first-difference ((t (:inherit unspecified)))))

(use-package bindings
  :straight nil
  :bind ( :map ctl-x-map
          ("C-d" . dired-jump)
          :map narrow-map
          ("i d" . indirect-narrow-to-defun)
          ("i n" . indirect-narrow-to-region)))

;;; UI

(use-package frame
  :straight nil
  :requires seq
  :config
  (define-advice toggle-frame-fullscreen
      (:before (&optional frame))
    "Hide menu bar when FRAME goes full screen."
    (set-frame-parameter
     nil 'menu-bar-lines
     (if (memq (frame-parameter frame 'fullscreen) '(fullscreen fullboth)) 1 0)))
  (define-advice switch-to-buffer-other-frame
      (:around (fn buffer-or-name &optional norecord))
    "Clone fame parameters when switching to another frame."
    (let* ((default-frame-alist
            (seq-remove (lambda (elem) (eq (car elem) 'name))
                        (frame-parameters (selected-frame)))))
      (funcall-interactively fn buffer-or-name norecord))))

(use-package startup
  :straight nil
  :no-require t
  :custom
  (inhibit-splash-screen t))

(use-package menu-bar
  :straight nil
  :unless (display-graphic-p)
  :config
  (menu-bar-mode -1))

(use-package tooltip
  :straight nil
  :when (window-system)
  :custom
  (tooltip-x-offset 0)
  (tooltip-y-offset local-config-line-pixel-height)
  (tooltip-frame-parameters `((name . "tooltip")
                              (internal-border-width . 2)
                              (border-width . 1)
                              (no-special-glyphs . t))))

(use-package modus-themes
  :requires (functions local-config)
  :functions (gnome-dark-mode-enabled-p in-termux-p)
  :defines (local-config-dark-theme local-config-light-theme)
  :custom-face
  (font-lock-doc-face ((t (:foreground nil :inherit font-lock-comment-face))))
  :custom
  (modus-themes-org-blocks nil)
  (modus-themes-syntax '(faint alt-syntax))
  (modus-themes-region '(bg-only no-extend))
  (modus-themes-operandi-color-overrides '((bg-main . "#fcfbfa") (fg-main . "#101010")))
  (modus-themes-vivendi-color-overrides (if (in-termux-p)
                                            '((bg-main . "#000000") (fg-main . "#e5e6e7"))
                                          '((bg-main . "#252423") (fg-main . "#dedddc"))))
  (modus-themes-completions 'opinionated)
  (modus-themes-mode-line '(moody borderless))
  :init
  (defun in-termux-p ()
    "Detect if Emacs is running in Termux."
    (executable-find "termux-info"))
  (defun gnome-dark-mode-enabled-p ()
    "Check if frame is dark or not."
    (if (executable-find "gsettings")
        (thread-last "gsettings get org.gnome.desktop.interface gtk-theme"
                     shell-command-to-string
                     string-trim-right
                     (string-suffix-p "-dark'"))
      (eq 'dark (frame-parameter nil 'background-mode))))
  (cond ((in-termux-p)
         (load-theme local-config-dark-theme t))
        ((gnome-dark-mode-enabled-p)
         (load-theme local-config-dark-theme t))
        (t (load-theme local-config-light-theme t))))

(use-package custom
  :straight nil
  :custom
  (custom-safe-themes t))

(use-package uniquify
  :straight nil
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package display-line-numbers
  :straight nil
  :custom
  (display-line-numbers-width 4)
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t))

(use-package formfeed
  :straight nil
  :preface
  (defun formfeed-make-display-line ()
    "Display the formfeed ^L char as a comment or as a continuous line."
    (unless buffer-display-table
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^L
          (vconcat (make-list (or fill-column 70)
                              (make-glyph-code
                               (string-to-char (or comment-start "-"))
                               'shadow)))))
  (provide 'formfeed)
  :init
  (dolist (mode-hook '(help-mode-hook
                       org-mode-hook
                       outline-mode-hook
                       prog-mode-hook))
    (add-hook mode-hook #'formfeed-make-display-line)))

(use-package pixel-scroll
  :straight nil
  :when (or (featurep 'xinput2)
            (featurep 'pgtk))
  :config
  (when (fboundp #'pixel-scroll-precision-mode)
    (setq-default scroll-margin 0)
    (pixel-scroll-precision-mode 1)))

;;; Completion

(use-package vertico
  :commands vertico-mode
  :bind ( :map vertico-map
          ("M-RET" . vertico-exit-input))
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :straight nil
  :load-path "straight/repos/vertico/extensions/"
  :bind ( :map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :commands marginalia-mode
  :init
  (marginalia-mode))

(use-package consult
  :commands consult-completion-in-region
  :preface
  (defvar consult-prefix-map (make-sparse-keymap))
  (fset 'consult-prefix-map consult-prefix-map)
  :bind ( :map ctl-x-map
          ("c" . consult-prefix-map)
          :map consult-prefix-map
          ("r" . consult-recent-file)
          ("o" . consult-outline)
          ("i" . consult-imenu)
          ("g" . consult-grep))
  :custom
  (consult-preview-key nil)
  :init
  (setq completion-in-region-function #'consult-completion-in-region))

(use-package corfu
  :straight ( :host github
              :repo "minad/corfu"
              :branch "main")
  :bind ( :map corfu-map
          ("TAB" . corfu-next)
          ([tab] . corfu-next)
          ("S-TAB" . corfu-previous)
          ([backtab] . corfu-previous)
          ([remap completion-at-point] . corfu-complete)
          ("RET" . corfu-complete-and-quit))
  :custom
  (corfu-cycle t)
  (corfu-preselect-first t)
  (corfu-scroll-margin 4)
  (corfu-quit-no-match t)
  (corfu-quit-at-boundary t)
  (corfu-max-width 100)
  (corfu-min-width 42)
  (corfu-count 9)
  ;; should be configured in the `indent' package,but `indent.el'
  ;; doesn't provide `indent'.
  (tab-always-indent 'complete)
  :config
  (defun corfu-complete-and-quit ()
    (interactive)
    (corfu-complete)
    (corfu-quit))
  :init
  (corfu-global-mode))

(use-package orderless
  :config
  (add-to-list 'completion-styles 'orderless))

(use-package cape
  :hook ((prog-mode cider-mode) . enable-cape)
  :commands (cape-dabbrev cape-file)
  :config
  (defun enable-cape ()
    (add-hook 'completion-at-point-functions #'cape-dabbrev 90 t)
    (add-hook 'completion-at-point-functions #'cape-file 90 t)))

;;;; Language packages

(use-package org
  :straight (:type built-in)
  :hook ((org-capture-mode org-src-mode) . discard-history)
  :bind ( :map org-mode-map
          ("M-Q" . split-pararagraph-into-lines)
          ("C-c l" . org-store-link))
  :custom-face
  (org-block ((t (:extend t))))
  (org-block-begin-line ((t ( :slant unspecified
                              :weight normal
                              :background unspecified
                              :inherit org-block
                              :extend t))))
  (org-block-end-line ((t ( :slant unspecified
                            :weight normal
                            :background unspecified
                            :inherit org-block-begin-line
                            :extend t))))
  (org-drawer ((t (:foreground nil :inherit shadow))))
  :custom
  (org-startup-with-inline-images nil)
  (org-tags-column -120)
  (org-startup-folded 'content)
  (org-hide-emphasis-markers nil)
  (org-adapt-indentation nil)
  (org-hide-leading-stars nil)
  (org-highlight-latex-and-related '(latex))
  (org-preview-latex-default-process 'dvisvgm)
  (org-src-fontify-natively t)
  (org-preview-latex-image-directory ".ltximg/")
  (org-confirm-babel-evaluate nil)
  (org-log-done 'time)
  (org-image-actual-width nil)
  (org-edit-src-content-indentation 0)
  :config
  (defun discard-history ()
    "Discard undo history of org src and capture blocks."
    (setq buffer-undo-list nil)
    (set-buffer-modified-p nil))
  (define-advice org-return (:around (f &rest args))
    (let ((org-src-preserve-indentation t))
      (apply f args)))
  (define-advice org-cycle (:around (f &rest args))
    (let ((org-src-preserve-indentation t))
      (apply f args)))
  (defun org-babel-edit-prep:emacs-lisp (_)
    "Setup Emacs Lisp buffer for Org Babel."
    (setq lexical-binding t)
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc))))

(use-package ox-hugo
  :after ox)

(use-package ox-latex
  :straight nil
  :after ox)

(with-eval-after-load 'org
  (use-package org-tempo
    :straight nil
    :defines org-version
    :unless (version<= org-version "9.1.9")))

(use-package cc-mode
  :straight nil
  :hook ((c-mode-common . cc-mode-setup))
  :config
  (defun cc-mode-setup ()
    (c-set-offset 'case-label '+)
    (setq c-basic-offset 4
          c-default-style "linux"
          comment-start "//"
          comment-end ""
          tab-width 4)))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind ( :map markdown-mode-map
          ("M-Q" . split-pararagraph-into-lines))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-command "pandoc")
  (markdown-hr-display-char nil)
  (markdown-list-item-bullets '("-")))

(use-package elisp-mode
  :straight nil
  :hook ((emacs-lisp-mode . eldoc-mode)
         (emacs-lisp-mode . common-lisp-modes-mode)))

(use-package fennel-mode
  :commands (fennel-repl lisp-eval-string lisp-eval-last-sexp switch-to-lisp)
  :hook ((fennel-mode fennel-repl-mode) . common-lisp-modes-mode)
  :bind ( :map fennel-mode-map
          ("C-c C-k" . eval-each-sexp)
          ("M-." . xref-find-definitions)
          ("M-," . xref-pop-marker-stack))
  :config
  (put 'global 'fennel-indent-function 1)
  (put 'local 'fennel-indent-function 1)
  (put 'var 'fennel-indent-function 1)
  (defvar org-babel-default-header-args:fennel '((:results . "silent")))
  (defun org-babel-execute:fennel (body _params)
    "Evaluate a block of Fennel code with Babel."
    (save-window-excursion
      (unless (bufferp fennel-repl--buffer)
        (fennel-repl nil))
      (let ((inferior-lisp-buffer fennel-repl--buffer))
        (lisp-eval-string body))))
  (defun eval-each-sexp ()
    "Evaluate each s-expression in the buffer consequentially.

If prefix ARG specified, call `fennel-reload' function.  If
double prefix ARG specified call `fennel-reload' function and ask
for the module name."
    (interactive)
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (while (save-excursion
                 (search-forward-regexp "[^[:space:]]." nil t))
          (forward-sexp)
          (when (and (not (nth 4 (syntax-ppss)))
                     (looking-back "." 1))
            (lisp-eval-last-sexp)))))
    (when fennel-mode-switch-to-repl-after-reload
      (switch-to-lisp t))))

(use-package clojure-mode
  :hook ((clojure-mode
          clojurec-mode
          clojurescript-mode)
         . clojure-mode-setup)
  :commands clojure-project-dir
  :functions clojure-set-compile-command
  :config
  (defvar org-babel-default-header-args:clojure '((:results . "silent")))'
  (defun org-babel-execute:clojure (body params)
    "Evaluate a block of Clojure code with Babel."
    (lisp-eval-string body))
  (defun clojure-set-compile-command ()
    (let ((project-dir (clojure-project-dir)))
      (cond ((and (file-exists-p (expand-file-name "project.clj" project-dir))
                  (executable-find "lein"))
             (setq-local compile-command "lein "))
            ((and (file-exists-p (expand-file-name "deps.edn" project-dir))
                  (executable-find "clojure"))
             (setq-local compile-command "clojure ")))))
  (defun clojure-mode-setup ()
    "Setup Clojure buffer."
    (common-lisp-modes-mode)
    (clojure-set-compile-command)
    (flycheck-mode)))

(use-package inferior-clojure
  :straight nil
  :requires (inf-lisp comint clojure-mode)
  :hook ((inferior-clojure-mode . common-lisp-modes-mode))
  :preface
  (require 'inf-lisp)
  (require 'comint)
  (require 'clojure-mode)
  (defgroup inferior-clojure ()
    "Support for interacting with Clojure via an inferior process."
    :prefix "inferior-clojure-"
    :group 'languages)
  (defcustom inferior-clojure-program "clojure"
    "Path to the program used by `inferior-clojure'"
    :tag "Clojure CLI"
    :type 'string
    :group 'inferior-clojure)
  (defcustom inferior-clojure-prompt "^[^=]+=> "
    "Prompt regex for inferior Clojure."
    :tag "Prompt regexp"
    :type 'string
    :group 'inferior-clojure)
  (defun inferior-clojure-indent-on-newline ()
    (interactive)
    (let (electric-indent-mode)
      (newline-and-indent)))
  (defun inferior-clojure (cmd)
    "Run an inferior instance of `inferior-clojure' inside Emacs."
    (interactive (list (if current-prefix-arg
			   (read-string "Run Clojure: " inferior-clojure-program)
		         inferior-clojure-program)))
    (if (not (comint-check-proc "*inferior-clojure*"))
        (let ((cmdlist (if (fboundp #'split-string-shell-command)
                           (split-string-shell-command cmd)
                         ;; NOTE: Less accurate, may be problematic
                         (split-string cmd))))
	  (set-buffer (apply (function make-comint)
			     "inferior-clojure" (car cmdlist) nil (cdr cmdlist)))
	  (inferior-clojure-mode)))
    (setq inferior-lisp-buffer "*inferior-clojure*")
    (pop-to-buffer-same-window "*inferior-clojure*"))
  (defalias 'run-clojure 'inferior-clojure)
  (define-derived-mode inferior-clojure-mode inferior-lisp-mode "Inferior Clojure"
    "Major mode for `inferior-clojure'.

\\<inferior-clojure-mode-map>"
    (setq-local font-lock-defaults '(clojure-font-lock-keywords t))
    (setq-local inferior-lisp-prompt inferior-clojure-prompt)
    (setq-local lisp-indent-function 'clojure-indent-function)
    (setq-local lisp-doc-string-elt-property 'clojure-doc-string-elt)
    (setq-local comint-prompt-read-only t)
    (setq-local comment-end "")
    (clojure-font-lock-setup)
    (make-local-variable 'completion-at-point-functions)
    (set-syntax-table clojure-mode-syntax-table)
    (add-hook 'paredit-mode-hook #'clojure-paredit-setup nil t))
  (define-key inferior-clojure-mode-map (kbd "C-j") 'inferior-clojure-indent-on-newline)
  (provide 'inferior-clojure))

(use-package cider
  :hook (((cider-repl-mode cider-mode) . eldoc-mode)
         (cider-repl-mode . common-lisp-modes-mode))
  :bind ( :map cider-repl-mode-map
          ("C-c C-S-o" . cider-repl-clear-buffer))
  :custom-face
  (cider-result-overlay-face ((t (:box (:line-width -1 :color "grey50")))))
  (cider-error-highlight-face ((t (:inherit flymake-error))))
  (cider-warning-highlight-face ((t (:inherit flymake-warning))))
  :custom
  (nrepl-log-messages nil)
  (cider-repl-display-help-banner nil)
  (cider-repl-tab-command #'indent-for-tab-command)
  (nrepl-hide-special-buffers t)
  (cider-test-show-report-on-success t)
  (cider-allow-jack-in-without-project t)
  (cider-use-fringe-indicators nil)
  (cider-font-lock-dynamically '(macro var deprecated))
  (cider-save-file-on-load nil)
  (cider-inspector-fill-frame nil)
  (cider-auto-select-error-buffer t)
  (cider-eval-spinner t)
  (nrepl-use-ssh-fallback-for-remote-hosts t)
  (cider-enrich-classpath t)
  :config
  (setq cider-jdk-src-paths nil)
  (dolist (src (append (file-expand-wildcards "/usr/lib/jvm/java-*-openjdk/src.zip")
                       (file-expand-wildcards "/usr/lib/jvm/java-*-openjdk/lib/src.zip")
                       (file-expand-wildcards "~/.clojure/clojure-*-sources.jar")))
    (when (file-exists-p src)
      (unless (memq src cider-jdk-src-paths)
        (add-to-list 'cider-jdk-src-paths src t)))))

(use-package flycheck-clj-kondo
  :when (executable-find "clj-kondo"))

(use-package clj-refactor
  :hook ((clj-refactor-mode . yas-minor-mode)
         (cider-mode . clj-refactor-mode))
  :custom
  (cljr-suppress-no-project-warning t)
  (cljr-suppress-middleware-warnings t)
  (cljr-warn-on-eval nil))

(use-package lisp-mode
  :straight nil
  :hook (lisp-mode . common-lisp-modes-mode))

(use-package inf-lisp
  :straight nil
  :hook (inferior-lisp-mode . common-lisp-modes-mode)
  :custom
  (inferior-lisp-program (cond ((executable-find "sbcl") "sbcl")
                               ((executable-find "ecl") "ecl"))))

(use-package sly
  :after inf-lisp
  :commands (sly-symbol-completion-mode sly-completing-read)
  :hook (sly-mrepl-mode . common-lisp-modes-mode)
  :config
  (sly-symbol-completion-mode -1))

(use-package geiser
  :hook (scheme-mode . geiser-mode)
  :custom
  (geiser-active-implementations '(guile))
  (geiser-default-implementation 'guile))

(use-package geiser-guile)

(use-package racket-mode
  :hook ((racket-mode racket-repl-mode) . common-lisp-modes-mode))

(use-package yaml-mode
  :custom
  (yaml-indent-offset 4))

(use-package sh-script
  :straight nil
  :hook (sh-mode . flycheck-mode))

(use-package lua-mode
  :commands (lua-get-create-process lua-send-string)
  :hook (lua-mode . flycheck-mode)
  :custom
  (lua-indent-level 2)
  :config
  (defvar org-babel-default-header-args:lua '((:results . "silent")))
  (defun org-babel-execute:lua (body _)
    "Evaluate a block of Lua code with Babel."
    (lua-get-create-process)
    (lua-send-string body)))

(use-package css-mode
  :straight nil
  :custom
  (css-indent-offset 2))

(use-package json-mode
  :hook (json-mode . flycheck-mode)
  :custom
  (js-indent-level 2))

(use-package csv-mode
  :custom
  (csv-align-max-width 80))

(use-package erlang)

(use-package elixir-mode)

;;; Tools

(use-package help
  :straight nil
  :custom
  (help-window-select t))

(use-package doc-view
  :straight nil
  :custom
  (doc-view-resolution 192))

(use-package vterm
  :if (bound-and-true-p module-file-suffix)
  :bind ( :map vterm-mode-map
          ("<insert>" . ignore)
          ("<f2>" . ignore)
          :map project-prefix-map
          ("t" . vterm-project-dir))
  :requires project
  :commands (vterm project-root)
  :custom
  (vterm-always-compile-module t)
  (vterm-environment '("VTERM=1"))
  :config
  (defun vterm-project-dir (&optional arg)
    (interactive "P")
    (let ((default-directory (project-root (project-current t))))
      (funcall-interactively #'vterm arg))))

(use-package flymake
  :straight nil
  :custom
  (flymake-fringe-indicator-position 'right-fringe))

(use-package flyspell
  :when (or (executable-find "ispell")
            (executable-find "aspell")
            (executable-find "hunspell"))
  :hook ((org-mode git-commit-mode markdown-mode) . flyspell-mode))

(use-package flycheck
  :defines (flymake-error-bitmap
            flymake-warning-bitmap
            flymake-note-bitmap)
  :commands (flycheck-define-error-level)
  :functions (flycheck-count-errors)
  :custom
  (flycheck-indication-mode 'right-fringe)
  (flycheck-display-errors-delay 86400 "86400 seconds is 1 day")
  (flycheck-emacs-lisp-load-path 'inherit)
  :config
  (flycheck-define-error-level 'error
    :severity 100
    :compilation-level 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap flymake-error-bitmap
    :fringe-face 'flycheck-fringe-error
    :error-list-face 'flycheck-error-list-error)
  (flycheck-define-error-level 'warning
    :severity 10
    :compilation-level 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap flymake-warning-bitmap
    :fringe-face 'flycheck-fringe-warning
    :error-list-face 'flycheck-error-list-warning)
  (flycheck-define-error-level 'info
    :severity -10
    :compilation-level 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap flymake-note-bitmap
    :fringe-face 'flycheck-fringe-info
    :error-list-face 'flycheck-error-list-info)
  (define-advice flycheck-mode-line-status-text (:override (&optional status))
    "Get a text describing STATUS for use in the mode line.

STATUS defaults to `flycheck-last-status-change' if omitted or
nil."
    (concat " " flycheck-mode-line-prefix ":"
            (pcase (or status flycheck-last-status-change)
              (`not-checked "-/-")
              (`no-checker "-")
              (`running "*/*")
              (`errored "!")
              (`finished
               (let-alist (flycheck-count-errors flycheck-current-errors)
                 (format "%s/%s" (or .error 0) (or .warning 0))))
              (`interrupted ".")
              (`suspicious "?"))))
  (define-advice flycheck-may-use-echo-area-p (:override ())
    nil))

(use-package flycheck-package)

(use-package smartparens
  :commands (sp-use-paredit-bindings sp-local-pair sp-update-local-pairs)
  :defines common-lisp-modes-mode-map
  :hook (((common-lisp-modes-mode
           prog-mode
           reb-mode
           reb-lisp-mode)
          . smartparens-strict-mode)
         ((eval-expression-minibuffer-setup
           lisp-data-mode)
          . minibuffer-enable-sp))
  :bind ( :map smartparens-mode-map
          ("C-M-q" . sp-indent-defun)
          :map common-lisp-modes-mode-map
          (";" . sp-comment))
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  (sp-show-pair-delay 0)
  (sp-echo-match-when-invisible nil)
  :config
  (require 'smartparens-config)
  (sp-use-paredit-bindings)
  (define-key smartparens-mode-map (kbd "M-r") 'sp-rewrap-sexp) ; needs to be set manually, because :bind section runs before :config
  (defun minibuffer-enable-sp ()
    "Enable `smartparens-strict-mode' in the minibuffer, during `eval-expression'."
    (setq-local comment-start ";")
    (sp-local-pair 'minibuffer-pairs "'" nil :actions nil)
    (sp-local-pair 'minibuffer-pairs "`" nil :actions nil)
    (sp-update-local-pairs 'minibuffer-pairs)
    (smartparens-strict-mode 1)))

(use-package undo-tree
  :commands (global-undo-tree-mode undo-tree-undo undo-tree-redo)
  :bind (("C-z" . undo-tree-undo)
         ("C-S-z" . undo-tree-redo))
  :custom
  (undo-tree-visualizer-relative-timestamps nil)
  (undo-tree-visualizer-timestamps nil)
  (undo-tree-auto-save-history nil)
  :init
  (global-undo-tree-mode 1))

(use-package magit
  :hook ((git-commit-mode . flyspell-mode))
  :custom
  (magit-ediff-dwim-show-on-hunks t)
  (magit-diff-refine-ignore-whitespace t)
  (magit-diff-refine-hunk 'all))

(use-package magit-todos
  :commands magit-todos-mode
  :after magit
  :custom
  (magit-todos-nice (when (executable-find "nice") t)
                    "avoid breaking Magit on systems that don't have `nice'")
  :init
  (let ((inhibit-message t))
    (magit-todos-mode 1))
  :config
  (transient-append-suffix 'magit-status-jump '(0 0 -1)
    '("T " "Todos" magit-todos-jump-to-todos)))

(use-package ediff
  :straight nil
  :hook (ediff-prepare-buffer . outline-show-all)
  :config
  (advice-add 'ediff-window-display-p :override #'ignore)
  :custom
  (ediff-split-window-function 'split-window-horizontally))

(use-package project
  :functions (project-root-p
              project-find-root)
  :commands (project-buffers project-compile)
  :bind ( :map project-prefix-map
          ("s" . project-save-some-buffers))
  :custom
  (project-compilation-buffer-name-function 'project-prefixed-buffer-name)
  :preface
  (defcustom project-root-markers
    '("Cargo.toml" "compile_commands.json" "compile_flags.txt" "project.clj" ".git" "deps.edn")
    "Files or directories that indicate the root of a project."
    :type '(repeat string)
    :group 'project)
  :config
  (unless (boundp 'project-switch-commands)
    (defvar project-switch-commands nil))
  (defun project-root-p (path)
    "Check if the current PATH has any of the project root markers."
    (memq t (mapcar (lambda (file)
                      (file-exists-p (concat path file)))
                    project-root-markers)))
  (defun project-find-root (path)
    "Recursive search in PATH for root markers."
    (cond
     ((project-root-p path) (cons 'transient path))
     ((equal "/" path) nil)
     (t (project-find-root
         (file-name-directory
          (directory-file-name path))))))
  (add-to-list 'project-find-functions #'project-find-root)
  (define-advice project-compile (:around (fn))
    "Only ask to save project-related buffers."
    (let* ((project-buffers (project-buffers (project-current)))
           (compilation-save-buffers-predicate
            (lambda () (memq (current-buffer) project-buffers))))
      (funcall fn)))
  (defun project-save-some-buffers (&optional arg)
    "Save some modified file-visiting buffers in the current project.

Optional argument ARG (interactively, prefix argument) non-nil
means save all with no questions."
    (interactive "P")
    (let* ((project-buffers (project-buffers (project-current)))
           (pred (lambda () (memq (current-buffer) project-buffers))))
      (funcall-interactively #'save-some-buffers arg pred))))

(use-package server
  :straight nil
  :config
  (unless (server-running-p)
    (server-start)))

(use-package separedit
  :hook (separedit-buffer-creation . separedit-header-line-setup)
  :bind ( :map prog-mode-map
          ("C-c '" . separedit)
          :map separedit-mode-map
          ("C-c C-c" . separedit-commit)
          :map edit-indirect-mode-map
          ("C-c '" . separedit))
  :custom
  (separedit-default-mode 'markdown-mode)
  :config
  (defun separedit-header-line-setup ()
    (setq-local
     header-line-format
     (substitute-command-keys
      "Edit, then exit with `\\[separedit-commit]' or abort with `\\[edit-indirect-abort]'"))))

(use-package recentf
  :straight nil
  :custom
  (recentf-max-menu-items 100)
  :config
  (add-to-list 'recentf-exclude "\\.gpg\\")
  (dolist (dir (list (expand-file-name ".cache/" user-emacs-directory)
                     (expand-file-name "workspace/.cache/" user-emacs-directory)))
    (add-to-list 'recentf-exclude (concat (regexp-quote dir) ".*")))
  (recentf-mode))

(use-package dumb-jump
  :commands dumb-jump-xref-activate
  :hook (prog-mode . dumb-jump-add-xref-backend)
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'completing-read)
  :config
  (defun dumb-jump-add-xref-backend ()
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate nil t)))

(use-package which-key
  :commands which-key-mode
  :init
  (which-key-mode t))

(use-package gcmh
  :commands gcmh-mode
  :init
  (gcmh-mode t))

(use-package paren
  :straight nil
  :hook (prog-mode . show-paren-mode))

(use-package vc-hooks
  :straight nil
  :custom
  (vc-follow-symlinks t))

(use-package isayt
  :straight ( :host gitlab
              :repo "andreyorst/isayt.el"
              :branch "main")
  :hook (common-lisp-modes-mode . isayt-mode))

(use-package eldoc
  :straight nil
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(use-package autorevert
  :straight nil
  :hook (after-init . global-auto-revert-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package compile
  :straight nil
  :hook (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-scroll-output 'first-error)
  :config
  (defmacro compile-add-error-syntax (name regexp file line &optional col level)
    "Register new compilation error syntax.

Add NAME symbol to `compilation-error-regexp-alist', and then add
REGEXP FILE LINE and optional COL LEVEL info to
`compilation-error-regexp-alist-alist'."
    (declare (indent 1))
    `(progn (add-to-list 'compilation-error-regexp-alist ',name)
            (add-to-list 'compilation-error-regexp-alist-alist
                         '(,name ,regexp ,file ,line ,col ,level))))
  (compile-add-error-syntax kaocha-tap
    "^not ok.*(\\([^:]*\\):\\([0-9]*\\))$"
    (1 "src/%s" "test/%s") 2)
  (compile-add-error-syntax kaocha-fail
    ".*FAIL in.*(\\([^:]*\\):\\([0-9]*\\))$"
    (1 "src/%s" "test/%s") 2)
  (compile-add-error-syntax clojure-reflection-warning
    "^Reflection warning,[[:space:]]*\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\).*$"
    (1 "src/%s" "test/%s") 2 3)
  (compile-add-error-syntax clojure-syntax-error
    "^Syntax error macroexpanding at (\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)).$"
    (1 "src/%s" "test/%s") 2 3)
  (compile-add-error-syntax lua-stacktrace
    "\\(?:^[[:space:]]+\\([^
:]+\\):\\([[:digit:]]+\\):[[:space:]]+in.+$\\)"
    1 2)
  (compile-add-error-syntax fennel-compile-error
    "\\(?:^Compile error in[[:space:]]+\\([^:]+\\):\\([[:digit:]]+\\)$\\)"
    1 2))

(use-package isearch
  :straight nil
  :bind ( :map isearch-mode-map
          ("<backspace>" . isearch-del-char)
          ("<left>" . isearch-edit-string)
          ("<right>" . isearch-edit-string)
          :map minibuffer-local-isearch-map
          ("<left>" . backward-char)
          ("<right>" . forward-char)))

(use-package esh-mode
  :straight nil
  :custom
  (eshell-scroll-show-maximum-output nil))

(use-package dired
  :straight nil
  :bind ( :map dired-mode-map
          ("<backspace>" . dired-up-directory))
  :custom
  (dired-listing-switches "-lAX --group-directories-first"))

(use-package comint
  :straight nil
  :custom
  (comint-scroll-show-maximum-output nil)
  (comint-highlight-input nil))

(use-package rect
  :straight nil
  :bind (("C-x r C-y" . rectangle-yank-add-lines))
  :config
  (defun rectangle-yank-add-lines ()
    (interactive "*")
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (save-restriction
      (narrow-to-region (point) (point))
      (yank-rectangle))))

(use-package jdecomp
  :mode ("\\.class\\'" . jdecomp-mode)
  :custom
  (jdecomp-decompiler-type (cond ((file-exists-p cfr-path)
                                  'cfr)
                                 ((file-exists-p fernflower-path)
                                  'fernflower)
                                 (t jdecomp-decompiler-type)))
  (jdecomp-decompiler-paths `((cfr . ,cfr-path)
                              (fernflower . ,fernflower-path)))
  :init
  (defvar cfr-path
    (file-truename "~/.local/lib/cfr.jar")
    "Path to the cfr Java decompiler library.")
  (defvar fernflower-path
    (file-truename "~/.local/lib/fernflower.jar")
    "Path to the FernFlower library."))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package phi-search)

(use-package multiple-cursors
  :defines region-bindings-mode-map
  :bind
  (("S-<mouse-1>" . mc/add-cursor-on-click)
   :map mc/keymap
   ("<return>" . nil)
   ("C-s" . phi-search)
   ("C-r" . phi-search-backward)
   ("C-&" . mc/vertical-align-with-space)
   :map region-bindings-mode-map
   ("n" . mc/mark-next-like-this)
   ("p" . mc/mark-previous-like-this)
   ("a" . mc/mark-all-like-this)
   ("s" . mc/mark-all-in-region-regexp)))

(provide 'init)
;;; init.el ends here

;; LocalWords:  init Bufname DEs config bufname Andrey Listopadov el
;; LocalWords:  clmm JetBrainsMono DejaVu unscaled minibuffer UI gtk
;; LocalWords:  tooltip Termux gsettings termux RET minad corfu ltximg
;; LocalWords:  formfeed src linux README md pandoc ARG Clojure clj ok
;; LocalWords:  lein clojure CLI kondo sbcl ecl Lua VTERM ispell Magit
;; LocalWords:  aspell hunspell Todos toml json txt edn deps gpg isayt
;; LocalWords:  workspace andreyorst macroexpanding cfr decompiler aoc
;; LocalWords:  FernFlower pkulev LocalWords
