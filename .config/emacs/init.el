;;; init.el --- Main configuration file -*- lexical-binding: t; no-byte-compile: t -*-

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

(use-package straight)

(use-package delight :straight t)

;;; Local config and personal functions

(use-package local-config
  :preface
  (defgroup local-config ()
    "Customization group for local settings."
    :prefix "local-config-"
    :group 'emacs)
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
  (defcustom openjdk-11-path
    "/usr/lib/jvm/java-11-openjdk/bin/java"
    "Path to OpenJDK 11 installation"
    :type 'string
    :tag "OpenJDK 11 Path"
    :group 'local-config)
  (defcustom langtool-installation-dir
    "~/.local/lib/LanguageTool-5.7-stable"
    "Path to the local installation of langtool."
    :type 'string
    :tag "Langtool installation directory"
    :group 'local-config)
  (defvar local-config-line-pixel-height (line-pixel-height)
    "Line height in pixels.

Used in various places to avoid getting wrong line height when
`text-scale-mode' is active.")
  (provide 'local-config))

(use-package functions
  :preface
  (require 'subr-x)
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
  (defun narrow-next-page ()
    "Narrow to the next page."
    (interactive)
    (widen)
    (unless (looking-at "")
      (forward-page))
    (narrow-to-page))
  (defun narrow-prev-page ()
    "Narrow to the previous page."
    (interactive)
    (widen)
    (unless (looking-at "")
      (backward-page))
    (backward-page)
    (narrow-to-page))
  (defun in-termux-p ()
    "Detect if Emacs is running in Termux."
    (executable-find "termux-info"))
  (defun dark-mode-enabled-p ()
    "Check if dark mode is enabled."
    (if (featurep 'dbus)
        (equal '1 (caar (dbus-call-method
                         :session
                         "org.freedesktop.portal.Desktop"
                         "/org/freedesktop/portal/desktop"
                         "org.freedesktop.portal.Settings"
                         "Read"
                         "org.freedesktop.appearance"
                         "color-scheme")))
      (eq 'dark (frame-parameter nil 'background-mode))))
  (defun edit-init-file ()
    (interactive)
    (find-file (expand-file-name "init.el" user-emacs-directory)))
  (defun edit-early-init-file ()
    (interactive)
    (find-file (expand-file-name "early-init.el" user-emacs-directory)))
  (defun clj-kondo-install (&optional install-dir)
    "Install clj-kondo using steps from the official installation script."
    (interactive (list (if current-prefix-arg
			   (expand-file-name (read-string "Install dir: " default-directory))
		         (expand-file-name "~/.local/bin"))))
    (let ((inhibit-message t)
          (tmp (make-temp-file "clj-kondo-install-"))
          (platform (cond ((string-equal system-type "darwin") "macos")
                          ((string-equal system-type "gnu/linux") "linux")
                          (t (user-error "Unsupported platform %s" system-type)))))
      (if (url-copy-file "https://raw.githubusercontent.com/clj-kondo/clj-kondo/master/resources/CLJ_KONDO_RELEASED_VERSION" tmp t)
          (let* ((version (with-temp-buffer
                            (insert-file-contents tmp)
                            (string-trim (buffer-string))))
                 (url (format "https://github.com/clj-kondo/clj-kondo/releases/download/v%s/clj-kondo-%s-%s-amd64.zip" version version platform))
                 (target (expand-file-name (format "%s/clj-kondo-%s.zip" install-dir version))))
            (if (url-copy-file url target t)
                (if (shell-command (format "unzip -qqo %s -d %s" target install-dir))
                    (let (inhibit-message)
                      (message "Installed clj-kondo v%s to %s" version install-dir))
                  (user-error "Unable to unzip %s" target))
              (user-error "Unable to download clj-kondo-%s-%s-amd64.zip" version platform)))
        (user-error "Unable to retrieve clj-kondo version"))))
  (provide 'functions))

(use-package kmacro
  :config
  (defun block-undo (fn &rest args)
    (let ((marker (prepare-change-group)))
      (unwind-protect (apply fn args)
        (undo-amalgamate-change-group marker))))
  (dolist (f '(kmacro-call-macro
               kmacro-exec-ring-item
               apply-macro-to-region-lines))
    (advice-add f :around #'block-undo)))

(use-package common-lisp-modes
  :delight  common-lisp-modes-mode
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
  (provide 'common-lisp-modes))

(use-package region-bindings
  :bind ( :map region-bindings-mode-map
          ("q" . region-bindings-disable)
          ("r" . replace-string)
          ("R" . replace-regexp))
  :preface
  (define-minor-mode region-bindings-mode
    "Minor mode for mapping commands while region is active.

\\<region-bindings-mode-map>"
    :lighter " rbm"
    :group 'convenience
    :keymap (make-sparse-keymap))
  (defun region-bindings-disable (&optional force)
    "Turn off bindings temporarely while keeping the region active.
Bindings will be enabled next time region is highlighted."
    (interactive)
    (region-bindings-mode -1))
  (defun region-bindings-enable ()
    "Enable bindings temporarely while keeping the region active."
    (interactive)
    (when (or transient-mark-mode
              (eq #'mouse-set-region this-command))
      (region-bindings-mode 1)))
  (defun region-bindings-mode-enable ()
    "Enable region bindings for all buffers."
    (interactive)
    (add-hook 'activate-mark-hook #'region-bindings-enable)
    (add-hook 'deactivate-mark-hook #'region-bindings-disable))
  (defun region-bindings-mode-disable ()
    "Disable region bindings."
    (interactive)
    (remove-hook 'activate-mark-hook #'region-bindings-enable)
    (remove-hook 'deactivate-mark-hook #'region-bindings-disable)
    (region-bindings-mode -1))
  (provide 'region-bindings)
  :init
  (region-bindings-mode-enable))

;;; Inbuilt stuff

(use-package defaults
  :preface
  (setq-default
   indent-tabs-mode nil
   truncate-lines t
   bidi-paragraph-direction 'left-to-right
   frame-title-format "Emacs"
   auto-window-vscroll nil
   mouse-highlight nil
   hscroll-step 1
   hscroll-margin 1
   scroll-margin 0
   scroll-preserve-screen-position nil
   scroll-conservatively 101
   frame-resize-pixelwise window-system
   window-resize-pixelwise window-system
   mode-line-format (list " "
                          '(:eval (propertize (if-let ((name (buffer-file-name))) (abbreviate-file-name name) (buffer-name))
                                              'face (when (and (buffer-file-name) (buffer-modified-p))
                                                      'font-lock-builtin-face)))
                          '(:eval (when current-input-method-title
                                    (propertize (concat " " current-input-method-title)
                                                'help-echo (concat "Input method: " current-input-method))))
                          (propertize " %l:%c"
                                      'help-echo "mouse-1: Goto line"
                                      'mouse-face 'mode-line-highlight
                                      'local-map (let ((map (make-sparse-keymap)))
                                                   (define-key map [mode-line down-mouse-1] 'goto-line)
                                                   map))
                          '(vc-mode vc-mode)
                          " "
                          mode-line-modes
                          " "
                          mode-line-misc-info))
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
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :init
  (load custom-file :noerror))

(use-package novice
  :init
  (defvar disabled-commands (expand-file-name "disabled.el" user-emacs-directory)
    "File to store disabled commands, that were enabled permanently.")
  (define-advice enable-command (:around (foo command) use-disabled-file)
    (let ((user-init-file disabled-commands))
      (funcall foo command)))
  (load disabled-commands :noerror))

(use-package startup
  :no-require t
  :custom
  (user-mail-address "andreyorst@gmail.com")
  (user-full-name "Andrey Listopadov"))

(use-package files
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
  :no-require t
  :init
  (fset 'yes-or-no-p 'y-or-n-p))

(use-package mwheel
  :bind (("S-<down-mouse-1>" . nil)
         ("S-<mouse-3>" . nil)
         ("<mouse-4>" . mwheel-scroll)
         ("<mouse-5>" . mwheel-scroll))
  :custom
  (mouse-wheel-flip-direction (not (featurep 'pgtk)))
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-progressive-speed nil)
  :init
  (if (fboundp #'context-menu-mode)
      (context-menu-mode 1)
    (global-set-key (kbd "<mouse-3>") menu-bar-edit-menu))
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
             (threshold (+ window-width hscroll-offset line-number-width
                           -2)))   ; compensate imprecise calculations
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
  (define-advice scroll-left (:before-while (&rest _) prevent-overscroll)
    (and truncate-lines
         (not (memq major-mode '(vterm-mode term-mode)))
         (truncated-lines-p)))
  (unless (display-graphic-p)
    (xterm-mouse-mode t)))

(use-package savehist
  :config
  (savehist-mode 1))

(use-package mule-cmds
  :no-require t
  :custom
  (default-input-method 'russian-computer)
  :init
  (prefer-coding-system 'utf-8))

(use-package select
  :when (display-graphic-p)
  :custom
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package simple
  :bind (("M-z" . zap-up-to-char)
         ("M-S-z" . zap-to-char)
         ("C-x k" . kill-this-buffer)
         ("C-h C-f" . describe-face)
         ([remap undo] . undo-only))
  :hook ((before-save . delete-trailing-whitespace)
         (overwrite-mode . overwrite-mode-set-cursor-shape))
  :custom
  (yank-excluded-properties t)
  (blink-matching-delay 0)
  (blink-matching-paren t)
  (copy-region-blink-delay 0)
  :init
  (column-number-mode 1)
  (line-number-mode 1)
  (defun overwrite-mode-set-cursor-shape ()
    (when (display-graphic-p)
      (setq cursor-type (if overwrite-mode 'hollow 'box))))
  (define-advice keyboard-quit (:around (quit) quit-current-context)
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
  :init
  (delete-selection-mode t))

(use-package minibuffer
  :hook (eval-expression-minibuffer-setup . common-lisp-modes-mode)
  :bind ( :map minibuffer-inactive-mode-map
          ("<mouse-1>" . ignore))
  :custom
  (completion-styles '(partial-completion basic))
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :custom-face
  (completions-first-difference ((t (:inherit unspecified)))))

(use-package orderless
  :straight t
  :custom
  (completion-category-overrides
   '((buffer (styles orderless))
     (file (styles orderless))
     (project-file (styles orderless)))))

(use-package bindings
  :bind ( :map ctl-x-map
          ("C-d" . dired-jump)
          :map narrow-map
          ("i d" . indirect-narrow-to-defun)
          ("i n" . indirect-narrow-to-region)
          ("]" . narrow-next-page)
          ("[" . narrow-prev-page))
  :init
  (setq mode-line-end-spaces nil))

;;; UI

(use-package frame
  :requires seq
  :bind (("C-z" . ignore)
         ("C-x C-z" . ignore))
  :config
  (define-advice toggle-frame-fullscreen
      (:before (&optional frame) hide-menu-bar)
    "Hide menu bar when FRAME goes full screen."
    (set-frame-parameter
     nil 'menu-bar-lines
     (if (memq (frame-parameter frame 'fullscreen) '(fullscreen fullboth)) 1 0)))
  (define-advice switch-to-buffer-other-frame
      (:around (fn buffer-or-name &optional norecord) clone-frame-parameters)
    "Clone fame parameters when switching to another frame."
    (let* ((default-frame-alist
            (seq-remove (lambda (elem) (eq (car elem) 'name))
                        (frame-parameters (selected-frame)))))
      (funcall-interactively fn buffer-or-name norecord))))

(use-package startup
  :no-require t
  :custom
  (inhibit-splash-screen t))

(use-package menu-bar
  :unless (display-graphic-p)
  :config
  (menu-bar-mode -1))

(use-package tooltip
  :when (window-system)
  :custom
  (tooltip-x-offset 0)
  (tooltip-y-offset local-config-line-pixel-height)
  (tooltip-frame-parameters `((name . "tooltip")
                              (internal-border-width . 2)
                              (border-width . 1)
                              (no-special-glyphs . t))))

(use-package dbus
  :when window-system
  :requires (functions local-config)
  :config
  (defun color-scheme-changed (path var value)
    "DBus handler to detect when the color-scheme has changed."
    (when (and (string-equal path "org.freedesktop.appearance")
               (string-equal var "color-scheme"))
      (if (equal (car value) '1)
          (load-theme local-config-dark-theme t)
        (load-theme local-config-light-theme t))))
  (dbus-register-signal :session
                        "org.freedesktop.portal.Desktop"
                        "/org/freedesktop/portal/desktop"
                        "org.freedesktop.portal.Settings"
                        "SettingChanged"
                        #'color-scheme-changed))

(use-package modus-themes
  :straight t
  :requires (functions local-config)
  :custom-face
  (font-lock-doc-face ((t (:foreground nil :inherit font-lock-comment-face))))
  (line-number ((t (:foreground nil :background nil :inherit shadow))))
  (line-number-current-line ((t (:foreground nil :weight bold :background nil :inherit hl-line))))
  :custom
  (modus-themes-org-blocks nil)
  (modus-themes-syntax '(faint alt-syntax))
  (modus-themes-region '(bg-only no-extend))
  (modus-themes-operandi-color-overrides '((bg-main . "#fcfaf8") (fg-main . "#151515")))
  (modus-themes-vivendi-color-overrides (if (in-termux-p)
                                            '((bg-main . "#000000") (fg-main . "#e5e6e7"))
                                          '((bg-main . "#1a1a1a") (fg-main . "#dbdbdb"))))
  (modus-themes-completions '((matches . (intense bold))
                              (selection . (intense))))
  (modus-themes-mode-line '(borderless))
  :init
  (cond ((in-termux-p)
         (load-theme local-config-dark-theme t))
        ((dark-mode-enabled-p)
         (load-theme local-config-dark-theme t))
        (t (load-theme local-config-light-theme t))))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward))

(use-package display-line-numbers
  :hook (display-line-numbers-mode . toggle-hl-line)
  :custom
  (display-line-numbers-width 4)
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t)
  :config
  (defun toggle-hl-line ()
    (hl-line-mode (if display-line-numbers-mode 1 -1))))

(use-package formfeed
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
  :when (or (featurep 'xinput2)
            (featurep 'pgtk))
  :config
  (when (fboundp #'pixel-scroll-precision-mode)
    (setq-default scroll-margin 0)
    (pixel-scroll-precision-mode 1)))

;;; Completion

(use-package vertico
  :straight t
  :bind ( :map vertico-map
          ("M-RET" . vertico-exit-input))
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :load-path "straight/repos/vertico/extensions/"
  :bind ( :map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

(use-package consult
  :straight t
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
          ("RET" . corfu-complete-and-quit)
          ("<return>" . corfu-complete-and-quit))
  :custom
  (corfu-cycle t)
  (corfu-preselect-first t)
  (corfu-scroll-margin 4)
  (corfu-quit-no-match t)
  (corfu-quit-at-boundary t)
  (corfu-max-width 100)
  (corfu-min-width 42)
  (corfu-count 9)
  ;; should be configured in the `indent' package, but `indent.el'
  ;; doesn't provide the `indent' feature.
  (tab-always-indent 'complete)
  :config
  (defun corfu-complete-and-quit ()
    (interactive)
    (corfu-complete)
    (corfu-quit))
  :init
  (global-corfu-mode))

(use-package corfu-doc
  :straight ( :host github
              :repo "galeo/corfu-doc"
              :branch "main")
  :bind ( :map corfu-map
          ("M-p" . corfu-doc-scroll-down)
          ("M-n" . corfu-doc-scroll-up))
  :hook (corfu-mode . corfu-doc-mode)
  :custom
  (corfu-doc-delay 2)
  (corfu-doc-max-height 20)
  (corfu-doc-max-width 84))

(use-package popon
  :straight ( :type git
              :repo "https://codeberg.org/akib/emacs-popon.git"
              :branch "master")
  :unless (display-graphic-p))

(use-package corfu-terminal
  :straight ( :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"
              :branch "master")
  :requires popon
  :unless (display-graphic-p)
  :config
  (corfu-terminal-mode 1))

(use-package corfu-doc-terminal
  :straight ( :type git
              :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git"
              :branch "master")
  :requires popon
  :unless (display-graphic-p)
  :config
  (corfu-doc-terminal-mode 1))

(use-package cape
  :straight t
  :config
  (setq completion-at-point-functions
        '(cape-file cape-dabbrev)))

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
  (org-tags-column -120)
  (org-startup-folded 'content)
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
  (define-advice org-return (:around (f &rest args) preserve-indentation)
    (let ((org-src-preserve-indentation t))
      (apply f args)))
  (define-advice org-cycle (:around (f &rest args) preserve-indentation)
    (let ((org-src-preserve-indentation t))
      (apply f args)))
  (defun org-babel-edit-prep:emacs-lisp (_)
    "Setup Emacs Lisp buffer for Org Babel."
    (setq lexical-binding t)))

(use-package ox-hugo
  :straight t
  :after ox)

(use-package ox-latex
  :after ox)

(with-eval-after-load 'org
  (use-package org-tempo
    :unless (version<= org-version "9.1.9")))

(use-package cc-mode
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
  :straight t
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
  :hook ((emacs-lisp-mode . eldoc-mode)
         (emacs-lisp-mode . common-lisp-modes-mode)))

(use-package fennel-mode
  :straight ( :branch "dynamic-font-lock"
              :repo "https://git.sr.ht/~technomancy/fennel-mode")
  :hook ((fennel-mode fennel-repl-mode) . common-lisp-modes-mode)
  :bind ( :map fennel-mode-map
          ("M-." . xref-find-definitions)
          ("M-," . xref-pop-marker-stack))
  :custom
  (fennel-eldoc-fontify-markdown t)
  :config
  (dolist (sym '(global local var))
    (put sym 'fennel-indent-function 1))
  (defvar org-babel-default-header-args:fennel '((:results . "silent")))
  (defun org-babel-execute:fennel (body _params)
    "Evaluate a block of Fennel code with Babel."
    (save-window-excursion
      (unless (bufferp fennel-repl--buffer)
        (fennel-repl nil))
      (let ((inferior-lisp-buffer fennel-repl--buffer))
        (lisp-eval-string body)))))

(use-package clojure-mode
  :straight t
  :hook ((clojure-mode
          clojurec-mode
          clojurescript-mode)
         . clojure-mode-setup)
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
    (common-lisp-modes-mode 1)
    (clojure-set-compile-command)))

(use-package cider
  :straight t
  :delight " CIDER"
  :hook (((cider-repl-mode cider-mode) . eldoc-mode)
         (cider-repl-mode . common-lisp-modes-mode)
         (cider-popup-buffer-mode . cider-disable-linting))
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
  (cider-show-eval-spinner t)
  (nrepl-use-ssh-fallback-for-remote-hosts t)
  (cider-enrich-classpath t)
  (cider-repl-history-file (expand-file-name "~/.cider-history"))
  (cider-clojure-cli-global-options "-J-XX:-OmitStackTraceInFastThrow")
  (cider-use-tooltips nil)
  (cider-connection-message-fn #'cider-random-tip)
  (cider-repl-prompt-function #'cider-repl-prompt-newline)
  :config
  (defun cider-disable-linting ()
    "Disable linting integrations for current buffer."
    (when flycheck-mode (flycheck-mode -1))
    (when flymake-mode (flymake-mode -1)))
  (defun cider-repl-prompt-newline (namespace)
    "Return a prompt string that mentions NAMESPACE with a newline."
    (format "%s\n> " namespace)))

(use-package clj-refactor
  :straight t
  :delight clj-refactor-mode
  :hook ((clj-refactor-mode . yas-minor-mode)
         (cider-mode . clj-refactor-mode))
  :custom
  (cljr-suppress-no-project-warning t)
  (cljr-suppress-middleware-warnings t)
  (cljr-warn-on-eval nil))

(use-package clj-decompiler
  :straight t
  :hook (cider-mode . clj-decompiler-setup))

(use-package lisp-mode
  :hook ((lisp-mode lisp-data-mode) . common-lisp-modes-mode))

(use-package inf-lisp
  :hook (inferior-lisp-mode . common-lisp-modes-mode)
  :bind ( :map common-lisp-modes-mode-map
          ("C-M-k" . lisp-eval-each-sexp))
  :custom
  (inferior-lisp-program (cond ((executable-find "sbcl") "sbcl")
                               ((executable-find "ecl") "ecl")))
  :init
  (defun lisp-eval-each-sexp ()
    "Evaluate each s-expression in the buffer consequentially."
    (interactive)
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (while (save-excursion
                 (search-forward-regexp "[^[:space:]]." nil t))
          (forward-sexp)
          (when (and (not (nth 4 (syntax-ppss)))
                     (looking-back "." 1))
            (lisp-eval-last-sexp)))))))

(use-package sly
  :straight t
  :after inf-lisp
  :hook (sly-mrepl-mode . common-lisp-modes-mode)
  :config
  (sly-symbol-completion-mode -1))

(use-package geiser
  :straight t
  :hook (scheme-mode . geiser-mode)
  :custom
  (geiser-active-implementations '(guile))
  (geiser-default-implementation 'guile))

(use-package geiser-guile :straight t)

(use-package racket-mode
  :straight t
  :hook ((racket-mode racket-repl-mode) . common-lisp-modes-mode))

(use-package yaml-mode
  :straight t
  :custom
  (yaml-indent-offset 4))

(use-package lua-mode
  :straight t
  :custom
  (lua-indent-level 2)
  :config
  (defvar org-babel-default-header-args:lua '((:results . "silent")))
  (defun org-babel-execute:lua (body _)
    "Evaluate a block of Lua code with Babel."
    (lua-get-create-process)
    (lua-send-string body)))

(use-package css-mode
  :custom
  (css-indent-offset 2))

(use-package json-mode
  :straight t
  :custom
  (js-indent-level 2))

(use-package csv-mode
  :straight t
  :custom
  (csv-align-max-width 80))

(use-package erlang
  :load-path "/usr/share/emacs/site-lisp/erlang/"
  :when (executable-find "erl"))

(use-package elixir-mode :straight t)

(use-package zig-mode :straight t)

(use-package scala-mode :straight t)

;;; Tools

(use-package help
  :custom
  (help-window-select t))

(use-package helpful
  :straight t
  :bind ( :map help-map
          ("f" . helpful-callable)
          ("v" . helpful-variable)
          ("k" . helpful-key)))

(use-package doc-view
  :custom
  (doc-view-resolution 192))

(use-package vterm
  :straight t
  :if (bound-and-true-p module-file-suffix)
  :bind ( :map vterm-mode-map
          ("<insert>" . ignore)
          ("<f2>" . ignore)
          :map project-prefix-map
          ("t" . vterm-project-dir))
  :requires project
  :custom
  (vterm-always-compile-module t)
  (vterm-environment '("VTERM=1"))
  :config
  (defun vterm-project-dir (&optional arg)
    "Launch vterm in current project.

Opens an existing vterm buffer for a project if present, unless
the prefix argument ARG is supplied."
    (interactive "P")
    (let* ((default-directory (project-root (project-current t)))
           (name (project-prefixed-buffer-name "vterm")))
      (if (and (not current-prefix-arg) (get-buffer name))
          (switch-to-buffer name)
        (funcall-interactively #'vterm name)))))

(use-package flymake
  :preface
  (defvar flymake-prefix-map (make-sparse-keymap))
  (fset 'flymake-prefix-map flymake-prefix-map)
  :bind ( :map ctl-x-map
          ("!" . flymake-prefix-map)
          :map flymake-prefix-map
          ("l" . flymake-show-buffer-diagnostics)
          ("n" . #'flymake-goto-next-error)
          ("p" . #'flymake-goto-prev-error))
  :custom
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-mode-line-lighter "FlyM")
  (flymake-mode-line-counter-format
   '(":" flymake-mode-line-error-counter "/"
     (:eval (flymake--mode-line-counter :warning 'no-space)))))

(use-package flyspell
  :straight t
  :when (or (executable-find "ispell")
            (executable-find "aspell")
            (executable-find "hunspell"))
  :hook ((org-mode git-commit-mode markdown-mode) . flyspell-mode))

(use-package smartparens
  :straight t
  :hook ((common-lisp-modes-mode . smartparens-strict-mode))
  :bind ( :map common-lisp-modes-mode-map
          (";" . sp-comment))
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  (sp-echo-match-when-invisible nil)
  :config
  (dolist (mode '(lisp-data-mode minibuffer-mode))
    (add-to-list 'sp-lisp-modes mode t)))

(use-package smartparens-config
  :config
  (sp-use-paredit-bindings)
  ;; needs to be set manually, because :bind section runs before :config
  (define-key smartparens-mode-map (kbd "M-r") 'sp-rewrap-sexp))

(use-package vundo
  :straight ( :host github
              :repo "casouri/vundo"
              :branch "master")
  :bind (("C-c u" . vundo))
  :custom
  (vundo-roll-back-on-quit nil)
  (vundo--window-max-height 10))

(use-package magit
  :straight t
  :hook (git-commit-mode . flyspell-mode)
  :custom
  (magit-ediff-dwim-show-on-hunks t)
  (magit-diff-refine-ignore-whitespace t)
  (magit-diff-refine-hunk 'all))

(use-package magit-todos
  :straight t
  :after magit
  :custom
  (magit-todos-nice (when (executable-find "nice") t)
                    "avoid breaking Magit on systems that don't have `nice'.")
  :init
  (let ((inhibit-message t))
    (magit-todos-mode 1))
  :config
  (transient-append-suffix 'magit-status-jump '(0 0 -1)
    '("T " "Todos" magit-todos-jump-to-todos)))

(use-package ediff
  :hook (ediff-prepare-buffer . outline-show-all)
  :config
  (advice-add 'ediff-window-display-p :override #'ignore)
  :custom
  (ediff-split-window-function 'split-window-horizontally))

(use-package project
  :straight t
  :bind ( :map project-prefix-map
          ("s" . project-save-some-buffers))
  :custom
  (project-compilation-buffer-name-function 'project-prefixed-buffer-name)
  :preface
  (defcustom project-root-markers
    '("Cargo.toml" "compile_commands.json" "compile_flags.txt"
      "project.clj" ".git" "deps.edn" "shadow-cljs.edn")
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
    "Search up the PATH for `project-root-markers'."
    (let ((path (expand-file-name path)))
      (catch 'found
        (while (not (equal "/" path))
          (if (not (project-root-p path))
              (setq path (file-name-directory (directory-file-name path)))
            (throw 'found (cons 'transient path)))))))
  (add-to-list 'project-find-functions #'project-find-root)
  (define-advice project-compile (:around (fn) save-project-buffers)
    "Only ask to save project-related buffers."
    (let* ((project-buffers (project-buffers (project-current)))
           (compilation-save-buffers-predicate
            (lambda () (memq (current-buffer) project-buffers))))
      (funcall fn)))
  (define-advice recompile (:around (fn &optional edit-command) save-project-buffers)
    "Only ask to save project-related buffers if inside of a project."
    (if (project-current)
        (let* ((project-buffers (project-buffers (project-current)))
               (compilation-save-buffers-predicate
                (lambda () (memq (current-buffer) project-buffers))))
          (funcall fn edit-command))
      (funcall fn edit-command)))
  (defun project-save-some-buffers (&optional arg)
    "Save some modified file-visiting buffers in the current project.

Optional argument ARG (interactively, prefix argument) non-nil
means save all with no questions."
    (interactive "P")
    (let* ((project-buffers (project-buffers (project-current)))
           (pred (lambda () (memq (current-buffer) project-buffers))))
      (funcall-interactively #'save-some-buffers arg pred))))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package separedit
  :straight t
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
  (nconc (assoc '(";+") separedit-comment-delimiter-alist)
         '(clojure-mode clojurec-mode clojure-script-mode))
  (define-advice separedit--point-at-comment (:before-while
                                              (&rest args)
                                              handle-docstring-comment-face)
    (not (apply 'separedit--point-at-string (cdr args))))
  (defun separedit-header-line-setup ()
    (setq-local
     header-line-format
     (substitute-command-keys
      "Edit, then exit with `\\[separedit-commit]' or abort with `\\[edit-indirect-abort]'"))))

(use-package recentf
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  :config
  (add-to-list 'recentf-exclude "\\.gpg\\")
  (dolist (dir (list (expand-file-name ".cache/" user-emacs-directory)
                     (expand-file-name "workspace/.cache/" user-emacs-directory)))
    (add-to-list 'recentf-exclude (concat (regexp-quote dir) ".*")))
  (recentf-mode))

(use-package dumb-jump
  :straight t
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'completing-read)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package gcmh
  :straight t
  :delight gcmh-mode
  :init
  (gcmh-mode t))

(use-package paren
  :hook (prog-mode . show-paren-mode))

(use-package vc-hooks
  :custom
  (vc-follow-symlinks t))

(use-package isayt
  :straight ( :host gitlab
              :repo "andreyorst/isayt.el"
              :branch "main")
  :delight isayt-mode
  :hook (common-lisp-modes-mode . isayt-mode))

(use-package eldoc
  :delight eldoc-mode
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

(use-package hl-todo
  :straight t
  :hook (prog-mode . hl-todo-mode))

(use-package compile
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-error-regexp-alist nil)
  :config
  (when (fboundp #'ansi-color-compilation-filter)
    (add-hook 'compilation-filter #'ansi-color-compilation-filter))
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
    1 2)
  (compile-add-error-syntax kaocha-fail
    ".*FAIL in.*(\\([^:]*\\):\\([0-9]*\\))$"
    1 2)
  (compile-add-error-syntax clojure-reflection-warning
    "^Reflection warning,[[:space:]]*\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\).*$"
    1 2 3)
  (compile-add-error-syntax clojure-syntax-error
    "^Syntax error macroexpanding at (\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)).$"
    1 2 3)
  (compile-add-error-syntax lua-stacktrace
    "\\(?:^[[:space:]]+\\([^
:]+\\):\\([[:digit:]]+\\):[[:space:]]+in.+$\\)"
    1 2)
  (compile-add-error-syntax fennel-compile-error
    "\\(?:^Compile error in[[:space:]]+\\([^:]+\\):\\([[:digit:]]+\\)$\\)"
    1 2))

(use-package isearch
  :bind ( :map isearch-mode-map
          ("<backspace>" . isearch-del-char)
          ("<left>" . isearch-edit-string)
          ("<right>" . isearch-edit-string)
          :map minibuffer-local-isearch-map
          ("<left>" . backward-char)
          ("<right>" . forward-char)))

(use-package esh-mode
  :custom
  (eshell-scroll-show-maximum-output nil))

(use-package dired
  :bind ( :map dired-mode-map
          ("<backspace>" . dired-up-directory)
          ("~" . dired-home-directory))
  :custom
  (dired-listing-switches "-lAX --group-directories-first")
  :config
  (defun dired-home-directory ()
    (interactive)
    (dired (expand-file-name "~/"))))

(use-package comint
  :custom
  (comint-scroll-show-maximum-output nil)
  (comint-highlight-input nil))

(use-package rect
  :bind (("C-x r C-y" . rectangle-yank-add-lines))
  :preface
  (defun rectangle-yank-add-lines ()
    (interactive "*")
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (save-restriction
      (narrow-to-region (point) (point))
      (yank-rectangle))))

(use-package jdecomp
  :straight t
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
  :straight t
  :bind ("C-=" . er/expand-region))

(use-package phi-search :straight t)

(use-package multiple-cursors
  :straight t
  :bind
  (("S-<mouse-1>" . mc/add-cursor-on-click)
   :map mc/keymap
   ("C-s" . phi-search)
   ("C-r" . phi-search-backward)
   ("C-&" . mc/vertical-align-with-space)
   ("C-#" . mc/insert-numbers)
   ("<return>" . nil)
   :map region-bindings-mode-map
   ("n" . mc/mark-next-like-this)
   ("p" . mc/mark-previous-like-this)
   ("a" . mc/mark-all-like-this)
   ("s" . mc/mark-all-in-region-regexp)
   ("l" . mc/edit-ends-of-lines))
  :config
  (define-key mc/keymap (kbd "<return>") nil))

(use-package yasnippet
  :straight t
  :delight yas-minor-mode)

(use-package profiler
  :straight t
  :bind ("<f2>" . profiler-start-or-report)
  :init
  (defun profiler-start-or-report ()
    (interactive)
    (if (not (profiler-cpu-running-p))
        (profiler-start 'cpu)
      (profiler-report)
      (profiler-cpu-stop))))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :config
  (define-advice hs-toggle-hiding (:before (&rest _) move-point-to-mouse)
    "Move point to the location of the mouse pointer."
    (mouse-set-point last-input-event)))

(use-package treemacs
  :straight t
  :custom
  (treemacs-no-png-images t))

(use-package flymake
  :straight t)

(use-package lsp-mode
  :straight t
  :hook (lsp-mode . lsp-diagnostics-mode)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-diagnostics-provider :flymake)
  (lsp-completion-provider :none)
  (lsp-session-file (expand-file-name ".lsp-session" user-emacs-directory))
  (lsp-log-io nil)
  (lsp-keep-workspace-alive nil)
  (lsp-idle-delay 0.05)
  ;; core
  (lsp-enable-xref t)
  (lsp-auto-configure nil)
  (lsp-eldoc-enable-hover nil)
  (lsp-enable-dap-auto-configure nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-imenu nil)
  (lsp-enable-indentation nil)
  (lsp-enable-links nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-suggest-server-download nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color nil)
  ;; completion
  (lsp-completion-enable nil)
  (lsp-completion-enable-additional-text-edit nil)
  (lsp-enable-snippet nil)
  (lsp-completion-show-kind nil)
  ;; headerline
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-headerline-breadcrumb-enable-diagnostics nil)
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-workspace-status-enable nil)
  ;; lens
  (lsp-lens-enable nil)
  ;; semantic
  (lsp-semantic-tokens-enable nil))

(use-package lsp-clojure
  :after lsp-mode
  :demand
  :hook ((clojure-mode
          clojurec-mode
          clojurescript-mode)
         . lsp))

(use-package lsp-treemacs
  :straight t
  :custom
  (lsp-treemacs-theme "Iconless"))

(use-package lsp-java
  :straight t
  :requires lsp-mode
  :when (file-exists-p openjdk-11-path)
  :hook (java-mode . lsp)
  :custom
  (lsp-java-java-path openjdk-11-path))

(use-package lsp-metals
  :straight t
  :requires lsp-mode
  :hook (scala-mode . lsp)
  :custom
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off")))

(use-package langtool
  :straight t
  :when (file-exists-p langtool-installation-dir)
  :custom
  (langtool-language-tool-jar
   (expand-file-name "languagetool-commandline.jar"
                     langtool-installation-dir))
  :config
  (define-advice langtool-check-buffer (:around (fn &optional lang) fix-narrowing)
    (save-mark-and-excursion
      (unless (use-region-p)
        (let ((inhibit-message t))
          (mark-whole-buffer)))
      (funcall fn lang))))

;;; Mail

(use-package message
  :custom
  (message-kill-buffer-on-exit t))

(use-package smtpmail)

(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :when (executable-find "mu")
  :custom
  (mu4e-completing-read-function #'completing-read)
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-change-filenames-when-moving t)
  (mu4e-attachment-dir (expand-file-name "~/Downloads"))
  (mu4e-sent-messages-behavior 'delete)
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-view-show-images nil)
  (mu4e-view-show-addresses t)
  (mu4e-context-policy 'pick-first)
  (mu4e-update-interval (* 30 60))
  :config
  (when (featurep 'orderless)
    (define-advice mu4e-ask-maildir (:around (fn prompt) use-orderless)
      (let ((completion-styles (append completion-styles '(orderless))))
        (funcall fn prompt))))
  (define-advice mu4e-get-maildirs (:around (fn) filter-maildirs-based-on-context)
    "Filters maildirs for current active context based on maildir prefix."
    (let* ((context-vars (mu4e-context-vars (mu4e-context-current)))
           (current-maildir (alist-get 'mu4e-maildir-context context-vars)))
      (if current-maildir
          (cl-remove-if-not (lambda (maildir)
                              (string-prefix-p current-maildir maildir))
                            (funcall fn))
        (funcall fn))))
  (defun make-mu4e-context-matcher (match-str)
    (lambda (msg)
      (when msg
        (string-prefix-p match-str (mu4e-message-field msg :maildir)))))
  (defun make-context (ctx)
    (let ((inbox (plist-get ctx :inbox)))
      (make-mu4e-context
       :name (plist-get ctx :name)
       :match-func (make-mu4e-context-matcher inbox)
       :vars `((mu4e-sent-folder . ,(format "%s/Sent" inbox))
               (mu4e-drafts-folder . ,(format "%s/Drafts" inbox))
               (mu4e-trash-folder . ,(format "%s/Trash" inbox))
               (mu4e-refile-folder . ,(format "%s/Archive" inbox))
               (mu4e-compose-signature . ,(cl-getf ctx :signature user-full-name))
               (mu4e-maildir-context . ,inbox)

               (smtpmail-smtp-user . ,(plist-get ctx :smtp-name))
               (smtpmail-local-domain . ,(plist-get ctx :smtp-server))
               (smtpmail-smtp-server . ,(plist-get ctx :smtp-server))
               (smtpmail-smtp-service . ,(plist-get ctx :port))

               (user-mail-address . ,(plist-get ctx :address))
               (send-mail-function . smtpmail-send-it)))))
  (when (load (expand-file-name "mail-contexts.el" user-emacs-directory) 'noerror)
    (setq mu4e-contexts (mapcar #'make-context mail-contexts)
          user-mail-address (plist-get (car mail-contexts) :address)
          mu4e-personal-addresses (mapcar (lambda (ctx) (plist-get ctx :address))
                                          mail-contexts))))

(use-package message-view-patch
  :straight t
  :when (executable-find "mu")
  :hook (gnus-part-display-hook . message-view-patch-highlight))

(provide 'init)
;;; init.el ends here
