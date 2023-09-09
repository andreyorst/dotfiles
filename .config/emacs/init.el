;;; init.el --- Main configuration file -*- lexical-binding: t; no-byte-compile: t -*-

;; Author: Andrey Listopadov
;; Keywords: Emacs configuration
;; Homepage: https://gitlab.com/andreyorst/dotfiles.git

;;; Commentary:
;; Emacs 29.1+ configuration.

;;; Code:

(use-package use-package
  :no-require
  :custom
  (use-package-enable-imenu-support t))

(use-package early-init
  :no-require
  :unless (featurep 'early-init)
  :config
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

(use-package delight
  :ensure t)

(use-package local-config
  :defer t
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
  (defcustom openjdk-11-path nil
    "Path to OpenJDK 11 installation"
    :type '(choice (const :tag "not installed" nil)
                   string)
    :tag "OpenJDK 11 Path"
    :group 'local-config)
  (defcustom langtool-installation-dir nil
    "Path to the local installation of langtool."
    :type '(choice (const :tag "not installed" nil)
                   string)
    :tag "Langtool installation directory"
    :group 'local-config)
  (defcustom langtool-ngrams-dir-name nil
    "Path to the unzipped directory with ngram data for langtool."
    :type '(choice (const :tag "not installed" nil)
                   string)
    :tag "Langtool ngrams installation directory"
    :group 'local-config)
  (defcustom no-scroll-modes '(term-mode)
    "Major modes to disable horizontal scrolling."
    :tag "Modes to disable horizontal scrolling"
    :type '(repeat symbol)
    :group 'local-config)
  (defvar local-config-line-pixel-height (line-pixel-height)
    "Line height in pixels.

Used in various places to avoid getting wrong line height when
`text-scale-mode' is active.")
  (provide 'local-config))

(use-package functions
  :defer t
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
            (delete-char -1))))))
  (defun in-termux-p ()
    "Detect if Emacs is running in Termux."
    (executable-find "termux-info"))
  (defun termux-color-theme-dark-p ()
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "~/.termux/theme-variant"))
      (looking-at-p "dark")))
  (defun dark-mode-enabled-p ()
    "Check if dark mode is enabled."
    (cond ((in-termux-p)
           (termux-color-theme-dark-p))
          ((featurep 'dbus)
           (dbus-color-theme-dark-p))
          (t nil)))
  (defun memoize (fn)
    (let ((memo (make-hash-table :test 'equal)))
      (lambda (&rest args)
        (let ((value (gethash args memo)))
          (or value (puthash args (apply fn args) memo))))))
  (defvar-local ssh-tunnel-port nil)
  (put 'ssh-tunnel-port 'safe-local-variable #'numberp)
  (defun ssh-tunnel (host port &optional local-port)
    "Start an SSH tunnel from localhost to HOST:PORT.
If LOCAL-PORT is nil, PORT is used as local port."
    (interactive (list (read-string "host: " nil 'ssh-host-history)
                       (read-number "port: " ssh-tunnel-port 'ssh-port-history)
                       (when current-prefix-arg
                         (read-number "local port: " ssh-tunnel-port 'ssh-port-history))))
    (let ((name (if (and local-port (not (= local-port port)))
                    (format "*ssh-tunnel:%s:%s:%s" local-port host port)
                  (format "*ssh-tunnel:%s:%s" host port))))
      (async-shell-command
       (format "ssh -4 -N -L %s:localhost:%s %s" (or local-port port) port host)
       (concat " " name))))
  (provide 'functions))

(use-package defaults
  :defer t
  :preface
  (setq-default
   indent-tabs-mode nil
   load-prefer-newer t
   truncate-lines t
   bidi-paragraph-direction 'left-to-right
   frame-title-format "Emacs"
   auto-window-vscroll nil
   mouse-highlight t
   hscroll-step 1
   hscroll-margin 1
   scroll-margin 0
   scroll-preserve-screen-position nil
   frame-resize-pixelwise window-system
   window-resize-pixelwise window-system)
  (when (window-system)
    (setq-default
     x-gtk-use-system-tooltips nil
     cursor-type 'box
     cursor-in-non-selected-windows 'hbar))
  (setq
   ring-bell-function 'ignore
   mode-line-percent-position nil
   enable-recursive-minibuffers t)
  (when (version<= "27.1" emacs-version)
    (setq bidi-inhibit-bpa t))
  (provide 'defaults))


;;; Core packages

(use-package messages
  :preface
  (provide 'messages)
  :bind ( :map messages-buffer-mode-map
          ("C-c C-o" . messages-clear-buffer))
  :config
  (defun messages-clear-buffer ()
    "Clear the *Messages* buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max)))))

(use-package kmacro
  :defer t
  :preface
  (defun block-undo (fn &rest args)
    (let ((marker (prepare-change-group)))
      (unwind-protect (apply fn args)
        (undo-amalgamate-change-group marker))))
  :config
  (dolist (f '(kmacro-call-macro
               kmacro-exec-ring-item
               apply-macro-to-region-lines))
    (advice-add f :around #'block-undo)))

(use-package mouse
  :bind (("<mode-line> <mouse-2>" . nil)
         ("<mode-line> <mouse-3>" . nil)))

(use-package mode-line
  :defer t
  :preface
  (defvar mode-line-interactive-position
    `(line-number-mode
      (:propertize " %l:%C"
                   help-echo "mouse-1: Goto line"
                   mouse-face mode-line-highlight
                   local-map ,(let ((map (make-sparse-keymap)))
                                (define-key map [mode-line down-mouse-1] 'goto-line)
                                map)))
    "Mode line position with goto line binding.")
  (put 'mode-line-interactive-position 'risky-local-variable t)
  (fset 'abbreviate-file-name-memo (memoize #'abbreviate-file-name))
  (defvar mode-line-buffer-file-name
    '(:eval (propertize (if-let ((name (buffer-file-name)))
                            (abbreviate-file-name-memo name)
                          (buffer-name))
                        'help-echo (buffer-name)
                        'face (when (and (buffer-file-name) (buffer-modified-p))
                                'font-lock-builtin-face)))
    "Show file name if buffer is visiting a file, otherwise show
buffer name.  If file is modified, a `font-lock-builtin-face' is
applied to the name.")
  (put 'mode-line-buffer-file-name 'risky-local-variable t)
  (defvar mode-line-input-method
    '(:eval (when current-input-method-title
              (propertize (concat " " current-input-method-title)
                          'help-echo (concat "Input method: " current-input-method))))
    "Display input method name in the modeline.")
  (put 'mode-line-input-method 'risky-local-variable t)
  (defvar mode-line-buffer-encoding
    '(:eval (propertize
             (let ((sys (coding-system-plist buffer-file-coding-system)))
               (concat " " (if (memq (plist-get sys :category)
                                     '(coding-category-undecided coding-category-utf-8))
                               "UTF-8"
                             (upcase (symbol-name (plist-get sys :name))))))
             'help-echo 'mode-line-mule-info-help-echo
             'local-map mode-line-coding-system-map)))
  (put 'mode-line-buffer-encoding 'risky-local-variable t)
  (defvar mode-line-line-encoding
    '(:eval (when-let ((eol (pcase (coding-system-eol-type buffer-file-coding-system)
                              (0 "LF")
                              (1 "CRLF")
                              (2 "CR")
                              (_ nil))))
              (propertize
               (concat " " eol)
               'help-echo (format "Line ending style: %s"
                                  (pcase eol
                                    ("LF" "Unix style LF")
                                    ("CRLF" "DOS style CRLF")
                                    ("CR" "Mac style CR")
                                    (_ "Undecided")))
               'local-map (let ((map (make-sparse-keymap)))
                            (define-key map [mode-line mouse-1] 'mode-line-change-eol)
                            map)))))
  (put 'mode-line-line-encoding 'risky-local-variable t)
  (setq-default mode-line-format
                '(" " mode-line-buffer-file-name mode-line-input-method
                  mode-line-buffer-encoding mode-line-line-encoding
                  mode-line-interactive-position (vc-mode vc-mode) " "
                  mode-line-modes " " mode-line-misc-info))
  (provide 'mode-line))

(use-package font
  :hook (after-init . setup-fonts)
  :preface
  (defun font-installed-p (font-name)
    "Check if a font with FONT-NAME is available."
    (find-font (font-spec :name font-name)))
  (defun setup-fonts ()
    (cond ((font-installed-p "JetBrainsMono")
           (set-face-attribute 'default nil :font "JetBrainsMono"))
          ((font-installed-p "Source Code Pro")
           (set-face-attribute 'default nil :font "Source Code Pro")))
    (when (font-installed-p "DejaVu Sans")
      (set-face-attribute 'variable-pitch nil :font "DejaVu Sans")))
  (provide 'font))

(use-package cus-edit
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :init
  (load custom-file :noerror))

(use-package novice
  :preface
  (defvar disabled-commands (expand-file-name "disabled.el" user-emacs-directory)
    "File to store disabled commands, that were enabled permanently.")
  :config
  (define-advice enable-command (:around (fn command) use-disabled-file)
    (let ((user-init-file disabled-commands))
      (funcall fn command)))
  (load disabled-commands 'noerror))

(use-package files
  :preface
  (defvar backup-dir
    (expand-file-name ".cache/backups" user-emacs-directory)
    "Directory to store backups.")
  (defvar auto-save-dir
    (expand-file-name ".cache/auto-save/" user-emacs-directory)
    "Directory to store auto-save files.")
  :custom
  (backup-by-copying t)
  (create-lockfiles nil)
  (backup-directory-alist
   `(("." . ,backup-dir)))
  (auto-save-file-name-transforms
   `((".*" ,auto-save-dir t)))
  (auto-save-no-message t)
  (auto-save-interval 100)
  (require-final-newline t)
  :bind ("<f5>" . revert-buffer-quick)
  :init
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t)))

(use-package subr
  :no-require
  :init
  (if (boundp 'use-short-answers)
      (setq-default use-short-answers t)
    (fset 'yes-or-no-p 'y-or-n-p)))

(use-package mwheel
  :bind (("S-<down-mouse-1>" . nil)
         ("S-<mouse-3>" . nil)
         ("<mouse-4>" . mwheel-scroll)
         ("<mouse-5>" . mwheel-scroll))
  :custom
  (mouse-wheel-flip-direction (not (featurep 'pgtk)))
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-progressive-speed nil)
  :preface
  (defun truncated-lines-p ()
    "Non-nil if any line is longer than `window-width' + `window-hscroll'.

Returns t if any line exceeds the right border of the window.
Used for stopping scroll from going beyond the longest line.
Based on `so-long-detected-long-line-p'."
    (let ((buffer (current-buffer))
          (tabwidth tab-width))
      (or (> (buffer-size buffer) 1000000) ; avoid searching in huge buffers
          (with-temp-buffer
            (insert-buffer-substring buffer)
            (setq-local tab-width tabwidth)
            (untabify (point-min) (point-max))
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
                                 -2))) ; compensate imprecise calculations
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
                      (throw 'excessive t))))))))))
  :init
  (if (fboundp #'context-menu-mode)
      (context-menu-mode 1)
    (global-set-key (kbd "<mouse-3>") menu-bar-edit-menu))
  (define-advice scroll-left (:before-while (&rest _) prevent-overscroll)
    (and truncate-lines
         (not (memq major-mode no-scroll-modes))
         (truncated-lines-p)))
  (unless (display-graphic-p)
    (xterm-mouse-mode t)))

(use-package savehist
  :hook (after-init . savehist-mode))

(use-package mule-cmds
  :no-require
  :custom
  (default-input-method 'russian-computer)
  :init
  (prefer-coding-system 'utf-8))

(use-package select
  :no-require
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
  (shell-command-default-error-buffer "*Shell Command Errors*")
  :config
  (defun overwrite-mode-set-cursor-shape ()
    (when (display-graphic-p)
      (setq cursor-type (if overwrite-mode 'hollow 'box))))
  :preface
  (unless (fboundp 'minibuffer-keyboard-quit)
    (autoload #'minibuffer-keyboard-quit "delsel" nil t))
  (define-advice keyboard-quit
      (:around (quit) quit-current-context)
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
        (funcall-interactively quit))))
  (define-advice exchange-point-and-mark
      (:around (fn &optional arg) tmm)
    "Conditionally exchange point and mark.

Only exchange point and mark when `transient-mark-mode' is either
disabled, or enabled and the mark is active."
    (when (or (and transient-mark-mode
                   mark-active)
              (not transient-mark-mode))
      (funcall fn arg)))
  :init
  (column-number-mode 1)
  (line-number-mode 1))

(use-package delsel
  :hook (after-init . delete-selection-mode))

(use-package common-lisp-modes
  :vc (:url "https://gitlab.com/andreyorst/common-lisp-modes.el.git"))

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
  :ensure t
  :defer t
  :custom
  (completion-category-overrides
   '((buffer (styles basic orderless))
     (file (styles basic orderless))
     (project-file (styles basic orderless)))))

(use-package bindings
  :bind ( :map ctl-x-map
          ("C-d" . dired-jump))
  :init
  (setq mode-line-end-spaces nil))

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
  :no-require
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
  :when (featurep 'dbusbind)
  :requires (functions local-config)
  :commands (dbus-register-signal dbus-call-method)
  :preface
  (defun color-scheme-changed (path var value)
    "DBus handler to detect when the color-scheme has changed."
    (when (and (string-equal path "org.freedesktop.appearance")
               (string-equal var "color-scheme"))
      (if (equal (car value) '1)
          (load-theme local-config-dark-theme t)
        (load-theme local-config-light-theme t))))
  (defun dbus-color-theme-dark-p ()
    (equal '1 (caar (condition-case nil
                        (dbus-call-method
                         :session
                         "org.freedesktop.portal.Desktop"
                         "/org/freedesktop/portal/desktop"
                         "org.freedesktop.portal.Settings"
                         "Read"
                         "org.freedesktop.appearance"
                         "color-scheme")
                      (error nil)))))
  :init
  (dbus-register-signal :session
                        "org.freedesktop.portal.Desktop"
                        "/org/freedesktop/portal/desktop"
                        "org.freedesktop.portal.Settings"
                        "SettingChanged"
                        #'color-scheme-changed))

(use-package modus-themes
  :ensure t
  :requires (local-config)
  :custom
  (modus-themes-org-blocks nil)
  (modus-themes-completions
   '((matches . (intense bold))
     (selection . (intense))))
  (modus-operandi-palette-overrides
   '((bg-main "#fbfbfb")
     (string "#702f00")
     (bg-line-number-active "#f0f0f0")))
  (modus-vivendi-palette-overrides
   `((bg-main ,(if (in-termux-p) "#000000" "#181818"))
     (bg-line-number-active "#1e1e1e")
     (string "#f5aa80")))
  :custom-face
  (region ((t :extend nil))))

(use-package modus-themes
  :after modus-themes
  :no-require
  :custom
  (modus-themes-common-palette-overrides
   `(;; syntax
     (builtin magenta-faint)
     (keyword cyan-faint)
     (comment fg-dim)
     (constant blue-faint)
     (docstring fg-dim)
     (docmarkup fg-dim)
     (fnname magenta-faint)
     (preprocessor cyan-faint)
     (string red-faint)
     (type magenta-cooler)
     (variable blue-faint)
     (rx-construct magenta-faint)
     (rx-backslash blue-faint)
     ;; misc
     (bg-paren-match bg-ochre)
     (bg-region bg-inactive)
     (fg-region unspecified)
     ;; line-numbers
     (fg-line-number-active fg-main)
     (bg-line-number-inactive bg-main)
     (fg-line-number-inactive fg-dim)
     ;; modeline
     (border-mode-line-active unspecified)
     (border-mode-line-inactive unspecified)
     ;; links
     (underline-link unspecified)
     (underline-link-visited unspecified)
     (underline-link-symbolic unspecified)
     ,@modus-themes-preset-overrides-faint))
  :config
  (load-theme
   (if (dark-mode-enabled-p)
       local-config-dark-theme
     local-config-light-theme)
   t))

(use-package uniquify
  :defer t
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
  :hook ((help-mode
          org-mode
          outline-mode
          prog-mode)
         . formfeed-make-display-line)
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
  (provide 'formfeed))

(use-package pixel-scroll
  :when (fboundp #'pixel-scroll-precision-mode)
  :hook (after-init . pixel-scroll-precision-mode)
  :custom
  (scroll-margin 0))

(use-package paren
  :hook (prog-mode . show-paren-mode))

(use-package vc-hooks
  :defer t
  :custom
  (vc-follow-symlinks t))

(use-package eldoc
  :delight eldoc-mode
  :defer t
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(use-package esh-mode
  :hook (eshell-mode . common-lisp-modes-mode)
  :preface
  (declare-function eshell-search-path "ext:esh-ext")
  (defun eshell-prompt ()
    (let* ((date (propertize (format-time-string "%a %H:%M") 'face '(:inherit shadow)))
           (path (abbreviate-file-name default-directory))
           (branch (when (and (eshell-search-path "git")
                              (locate-dominating-file default-directory ".git"))
                     (concat (propertize (propertize " on " 'face '(:inherit shadow)))
                             (propertize (string-trim (shell-command-to-string "git branch --show-current"))
                                         'face (if (string-empty-p (shell-command-to-string "git status --porcelain 2>/dev/null"))
                                                   '(:inherit shadow)
                                                 '(:inherit font-lock-builtin-face))))))
           (container (cond
                       ((file-exists-p "/run/.containerenv")
                        (format " in %s"
                                (with-temp-buffer
                                  (save-match-data
                                    (insert-file-contents "/run/.containerenv")
                                    (re-search-forward "^name=\"\\([^\"]+\\)\"" nil t)
                                    (switch-to-buffer (current-buffer))
                                    (or (match-string-no-properties 1) "podman")))))
                       ((file-exists-p "/.dockerenv") " in docker")))
           (ssh (when (getenv "SSH_CONNECTION") " via ssh"))
           (info (concat (or branch "")
                         (propertize (concat (or container "")
                                             (or ssh ""))
                                     'face '(:inherit shadow))))
           (prompt (if (= eshell-last-command-status 0)
                       "$"
                     (propertize "$" 'face '(:inherit error)))))
      (concat date " " path info "\n" prompt " ")))
  :custom
  (eshell-scroll-show-maximum-output nil)
  (eshell-prompt-function 'eshell-prompt)
  (eshell-banner-message ""))

(use-package esh-module
  :after eshell
  :custom
  (eshell-modules-list
   (cl-remove 'eshell-term eshell-modules-list)))

(use-package dired
  :bind ( :map dired-mode-map
          ("<backspace>" . dired-up-directory)
          ("M-<up>" . dired-up-directory)
          ("~" . dired-home-directory))
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-listing-switches "-lAXhv --group-directories-first")
  :config
  (defun dired-home-directory ()
    (interactive)
    (dired (expand-file-name "~/"))))

(use-package comint
  :defer t
  :custom
  (comint-scroll-show-maximum-output nil)
  (comint-highlight-input nil)
  (comint-input-ignoredups t))

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

(use-package profiler
  :bind ("<f2>" . profiler-start-or-report)
  :commands (profiler-report)
  :preface
  (defun profiler-start-or-report ()
    (interactive)
    (if (not (profiler-cpu-running-p))
        (profiler-start 'cpu)
      (profiler-report)
      (profiler-cpu-stop))))

(use-package hideshow
  :hook (prog-mode . hs-minor-mode)
  :delight hs-minor-mode
  :bind ( :map hs-minor-mode-map
          ("C-c @ C-p" . hs-hide-all-private))
  :preface
  (defvar hs-mode-private-regex-alist
    `(((emacs-lisp-mode lisp-mode)
       . ,(rx bol "(def" (+ (not space)) (+ space) (+ (not space)) "--"))
      ((clojure-mode clojurescrip-mode clojurec-mode)
       . ,(rx "(" (or "defn-"
                      (seq "def" (* (not space)) (+ space)
                           "^" (or ":private"
                                   (seq "{" (* (not "}")) ":private" (+ space) "true")))
                      "comment")))
      (zig-mode
       . ,(rx bol (* space) "fn" (+ (not "{")) "{"))
      (fennel-mode
       . ,(rx bol "(" (or (seq (or "fn" "local" "var") (+ space) "-" alpha)
                          "comment"))))
    "Alist of major modes to regular expressions for finding private definitions")
  (defun hs-hide-all-private ()
    "Hide all private definitions in the current buffer.
Search is based on regular expressions in the
`hs-private-regex-mode-alist' variable."
    (interactive)
    (when hs-minor-mode
      (if-let ((re (alist-get major-mode hs-mode-private-regex-alist nil nil
                              (lambda (key1 key2)
                                (if (listp key1)
                                    (and (memq key2 key1) t)
                                  (eq key1 key2))))))
          (save-excursion
            (goto-char (point-max))
            (while (re-search-backward re nil t)
              (hs-hide-block)))
        (error "Mode %s doesn't define a regex to find private definitions" major-mode))))
  :config
  (easy-menu-add-item hs-minor-mode-map '(menu-bar hide/show)
                      ["Hide all private definitions" hs-hide-all-private
                       :help "Hide all private definitions based on `hs-mode-private-regex-alist'."]
                      "--")
  (define-advice hs-toggle-hiding (:before (&rest _) move-point-to-mouse)
    "Move point to the location of the mouse pointer."
    (mouse-set-point last-input-event)))

(use-package help
  :custom
  (help-window-select t))

(use-package doc-view
  :defer t
  :custom
  (doc-view-resolution 192))

(use-package flymake
  :preface
  (defvar flymake-prefix-map (make-sparse-keymap))
  (fset 'flymake-prefix-map flymake-prefix-map)
  :bind ( :map ctl-x-map
          ("!" . flymake-prefix-map)
          :map flymake-prefix-map
          ("l" . flymake-show-buffer-diagnostics)
          ("n" . flymake-goto-next-error)
          ("p" . flymake-goto-prev-error))
  :custom
  (flymake-fringe-indicator-position 'right-fringe)
  (flymake-mode-line-lighter "FlyM")
  (flymake-mode-line-counter-format
   '(":" flymake-mode-line-error-counter "/"
     (:eval (flymake--mode-line-counter :warning 'no-space))))
  :config
  (setq elisp-flymake-byte-compile-load-path (cons "./" load-path)))

(use-package package-lint-flymake
  :ensure t
  :defer t)

(use-package flyspell
  :ensure t
  :when (or (executable-find "ispell")
            (executable-find "aspell")
            (executable-find "hunspell"))
  :hook ((org-mode git-commit-mode markdown-mode) . flyspell-mode))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

(use-package outline
  :hook (common-lisp-modes-mode . lisp-outline-minor-mode)
  :custom
  (outline-minor-mode-cycle t)
  :preface
  (defun lisp-outline-minor-mode ()
    (setq-local outline-regexp "^;;;;*[[:space:]]\\w")
    (outline-minor-mode)))

(use-package face-remap
  :hook (text-scale-mode . text-scale-adjust-latex-previews)
  :preface
  (defun text-scale-adjust-latex-previews ()
    "Adjust the size of latex previews when changing text scale."
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (pcase major-mode
              ('latex-mode (eq (overlay-get ov 'category)
                               'preview-overlay))
              ('org-mode (eq (overlay-get ov 'org-overlay-type)
                             'org-latex-overlay)))
        (overlay-put
         ov 'display
         (cons 'image
               (plist-put
                (cdr (overlay-get ov 'display))
                :scale (+ 1.0 (* 0.25 text-scale-mode-amount)))))))))

(use-package browse-url
  :when (fboundp 'xwidget-webkit-browse-url)
  :custom (browse-url-browser-function #'xwidget-webkit-browse-url))


;;; Completion

(use-package vertico
  :ensure t
  :bind ( :map vertico-map
          ("M-RET" . vertico-exit-input))
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind ( :map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :ensure t
  :after vertico
  :config
  (marginalia-mode))

(use-package consult
  :ensure t
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
  :ensure t
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
  :hook (after-init . global-corfu-mode))

(use-package corfu-popupinfo
  :bind ( :map corfu-popupinfo-map
          ("M-p" . corfu-popupinfo-scroll-down)
          ("M-n" . corfu-popupinfo-scroll-up))
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom-face
  (corfu-popupinfo ((t :height 1.0))))

(use-package corfu-terminal
  :ensure t
  :unless (display-graphic-p)
  :after corfu
  :config
  (corfu-terminal-mode 1))

(use-package cape
  :ensure t
  :after corfu
  :config
  (setq completion-at-point-functions
        '(cape-file cape-dabbrev)))


;;; Org

(use-package org
  :hook ((org-babel-after-execute . org-redisplay-inline-images))
  :bind ( :map org-mode-map
          ("M-Q" . split-pararagraph-into-lines)
          ("C-c l" . org-store-link))
  :custom-face
  (org-block ((t (:extend t))))
  (org-block-begin-line
   ((t ( :slant unspecified
         :weight normal
         :background unspecified
         :inherit org-block
         :extend t))))
  (org-block-end-line
   ((t ( :slant unspecified
         :weight normal
         :background unspecified
         :inherit org-block-begin-line
         :extend t))))
  (org-drawer ((t (:foreground unspecified :inherit shadow))))
  :commands (org-return org-cycle)
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
  (defun org-babel-edit-prep:emacs-lisp (_)
    "Setup Emacs Lisp buffer for Org Babel."
    (setq lexical-binding t))
  (unless (version<= org-version "9.1.9")
    (add-to-list 'org-modules 'org-tempo)))

(use-package ob-shell :after org)

(use-package blog
  :commands (blog-publish-file
             blog-generate-file-name
             blog-read-list-items
             blog-new-post)
  :after ox-hugo
  :preface
  (defvar blog-capture-template
    "#+hugo_base_dir: ../
#+hugo_section: posts
#+hugo_auto_set_lastmod: t
#+options: tex:dvisvgm
#+macro: kbd @@html:<kbd>$1</kbd>@@
#+macro: a @@html:<a href=\"$1\">$2</a>@@

#+title: %(format \"%s\" blog--current-post-name)
#+date: %(format-time-string \"%Y-%m-%d %h %H:%M\")
#+hugo_tags: %(blog-read-list-items \"Select tags: \" 'blog-tags)
#+hugo_categories: %(blog-read-list-items \"Select categories: \" 'blog-categories)
#+hugo_custom_front_matter: :license %(format \"%S\" blog-license)
#+hugo_draft: true

%?"
    "Org-capture template for blog posts.")
  (defcustom blog-tags nil
    "A list of tags used for posts."
    :type '(repeat string)
    :group 'blog)
  (defcustom blog-categories nil
    "A list of tags used for posts."
    :type '(repeat string)
    :group 'blog)
  (defcustom blog-directory "~/blog"
    "Location of the blog directory for org-capture."
    :type 'string
    :group 'blog)
  (defcustom blog-license ""
    "Blog license string."
    :type 'string
    :group 'blog)
  (defvar blog--current-post-name nil
    "Current post name for org-capture template.")
  (defun blog-read-list-items (prompt var)
    "Completing read items with the PROMPT from the VAR.

VAR must be a quoted custom variable, which will be saved if new
items were read by the `completing-read' function."
    (let ((items (eval var)) item result)
      (while (not (string-empty-p item))
        (setq item (string-trim (or (completing-read prompt items) "")))
        (unless (string-empty-p item)
          (push item result)
          (setq items (remove item items))
          (unless (member item (eval var))
            (customize-save-variable var (sort (cons item (eval var)) #'string<)))))
      (string-join result " ")))
  (defun blog-title-to-fname (title)
    (thread-last
      title
      (replace-regexp-in-string "[[:space:]]" "-")
      (replace-regexp-in-string "-+" "-")
      (replace-regexp-in-string "[^[:alnum:]-]+" "")
      downcase))
  (defun blog-generate-file-name (&rest _)
    (let ((title (read-string "Title: ")))
      (setq blog--current-post-name title)
      (find-file
       (file-name-concat
        (expand-file-name blog-directory)
        "posts"
        (format "DRAFT-%s-%s.org"
                (format-time-string "%Y-%m-%d")
                (blog-title-to-fname title))))))
  (defun blog-publish-file ()
    "Update '#+date:' tag, and rename the currently visited file.
File name is updated to include the same date and current title.
Export the file to md with the `ox-hugo' package."
    (interactive)
    (save-match-data
      (let ((today (format-time-string "%Y-%m-%d"))
            (now (format-time-string "%h %H:%M")))
        (save-excursion
          (goto-char (point-min))
          (re-search-forward "^#\\+date:.*$")
          (replace-match (format "#+date: %s %s" today now))
          (re-search-forward "^#\\+hugo_draft:.*$")
          (replace-match "#+hugo_draft: false"))
        (let* ((file-name (save-excursion
                            (goto-char (point-min))
                            (re-search-forward "^#\\+title:[[:space:]]*\\(.*\\)$")
                            (blog-title-to-fname (match-string 1)))))
          (condition-case nil
              (rename-visited-file
               (format "%s-%s.org" today
                       (if (string-match
                            "^[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}-\\(.*\\)$"
                            file-name)
                           (match-string 1 file-name)
                         file-name)))
            (file-already-exists nil))
          (save-buffer)
          (org-hugo-export-to-md)))))
  (defun blog-new-post ()
    "Capture a new post."
    (interactive)
    (org-capture nil "p"))
  (defun blog-scour-svg (file)
    (if (executable-find "scour")
        (let ((tmp (make-temp-file "scour.svg.")))
          (with-temp-file tmp
            (insert-file-contents-literally file))
          (make-process :name "scour"
                        :connection-type 'pipe
                        :sentinel (lambda (_ _) (delete-file tmp))
                        :command (list "scour" "-i" tmp "-o" file)))
      (user-error "scour is not installed")))
  (defun blog-export-static-org-link (path description _backend _properties)
    "Export link to Markdown."
    (defvar org-hugo-base-dir)
    (let ((new-path (expand-file-name
                     (file-name-concat "../static" path)
                     org-hugo-base-dir)))
      (copy-file (expand-file-name path) new-path t)
      (format "[%s](/%s)"
              (or description path)
              (file-name-nondirectory path))))
  (defun blog-create-static-org-link (&optional _)
    "Create a file link using completion."
    (let ((file (read-file-name "File: "))
	  (pwd (file-name-as-directory (expand-file-name ".")))
	  (pwd1 (file-name-as-directory (abbreviate-file-name
				         (expand-file-name ".")))))
      (cond ((string-match
	      (concat "^" (regexp-quote pwd1) "\\(.+\\)") file)
	     (concat "org:" (match-string 1 file)))
	    ((string-match
	      (concat "^" (regexp-quote pwd) "\\(.+\\)")
	      (expand-file-name file))
	     (concat "org:"
		     (match-string 1 (expand-file-name file))))
	    (t (concat "org:" file)))))
  (require 'ol)
  (org-link-set-parameters
   "org"
   :export #'blog-export-static-org-link
   :complete #'blog-create-static-org-link)
  (defun blog-follow-html-link (path arg)
    (funcall browse-url-browser-function path arg))
  (defun blog-export-hmtl-link (path description _backend _properties)
    "Export link directly to HTML."
    (format "<a href=\"%s\">%s</a>" path (or description path)))
  (defun blog-create-html-link (&optional _)
    "Create a file link using completion."
    (let ((link (read-string "Link: ")))
      (concat "blog-html:" link)))
  (org-link-set-parameters
   "blog-html"
   :follow #'blog-follow-html-link
   :export #'blog-export-hmtl-link
   :complete #'blog-create-html-link)
  (require 'ox)
  (define-advice org-hugo-heading (:around (fn heading contents info) patch)
    (if (and (org-export-get-node-property :BLOG-COLLAPSABLE heading) (not (string-empty-p contents)))
        (let ((title (org-export-data (org-element-property :title heading) info)))
          (concat "<details class=\"foldlist\"><summary>" title
                  "</summary><div class=\"foldlistdata\">\n\n"
                  contents
                  "</div></details>"))
      (funcall fn heading contents info)))
  (provide 'blog))

(use-package org-capture
  :defer t
  :bind ("C-c o c" . org-capture)
  :custom
  (org-directory blog-directory)
  (org-capture-templates
   `(("p" "Post" plain
      (function blog-generate-file-name)
      ,blog-capture-template
      :jump-to-captured t
      :immediate-finish t)
     ,@(mapcar
        (lambda (spec)
          (seq-let (btn descr heading) spec
            `( ,btn ,descr entry
               (file+headline ,(expand-file-name "kb/index.org" blog-directory) ,heading)
               "* [[blog-html:%^{Link}][%^{Description}]]\n:properties:\n:blog-collapsable: t\n:end:"
               :immediate-finish t
               :before-finalize (org-hugo-export-to-md))))
        '(("a" "Article" "Articles")
          ("t" "Talk" "Talks")
          ("w" "Web page" "Various Web pages")
          ("b" "Books, courses" "Books, Courses"))))))

(use-package org-tree-slide
  :ensure t
  :defer t
  :custom
  (org-tree-slide-slide-in-effect nil)
  (org-tree-slide-never-touch-face t))

(use-package org-modern
  :ensure t
  :defer t
  :hook (org-tree-slide-mode . org-modern-mode)
  :custom-face
  (org-modern-block-name ((t (:height 1.0))))
  (org-modern-label ((t (:height 1.0))))
  :custom
  (org-modern-hide-stars t)
  (org-modern-block-fringe nil))

(use-package ox-hugo
  :ensure t
  :after ox)

(use-package ox-latex
  :after ox)


;;; Languages

(use-package cc-mode
  :hook (c-mode-common . cc-mode-setup)
  :custom
  (c-basic-offset 4)
  (c-default-style "linux")
  :config
  (defun cc-mode-setup ()
    (c-set-offset 'case-label '+)
    (setq-local comment-start "//"
                comment-end ""
                tab-width 4)))

(use-package markdown-mode
  :ensure t
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

(use-package racket-mode
  :ensure t
  :hook ((racket-mode racket-repl-mode) . common-lisp-modes-mode))

(use-package yaml-mode
  :ensure t
  :defer t
  :custom
  (yaml-indent-offset 4))

(use-package css-mode
  :defer t
  :custom
  (css-indent-offset 2))

(use-package json-mode
  :defer t
  :custom
  (js-indent-level 2))

(use-package csv-mode
  :ensure t
  :defer t
  :custom
  (csv-align-max-width 80))

(use-package scala-mode
  :ensure t
  :defer t)

(use-package zig-mode
  :ensure t)

(use-package abbrev
  :delight abbrev-mode
  :custom
  (save-abbrevs nil))

(use-package lua-mode
  :ensure t
  :hook (lua-mode . lua-setup-abbrev-prettify)
  :custom
  (lua-indent-level 2)
  :preface
  (defvar lua-syntax-expansions
    '(("func" "local function")
      ("fn"  "function")
      ("let" "local")
      ("<-" "return")))
  (defun lua-expand-abbrev-maybe ()
    (when (looking-back "<-" 2)
      (progn
        (delete-char -2)
        (abbrev-insert (abbrev-symbol "<-")))))
  (defun lua-setup-abbrev-prettify ()
    (setq prettify-symbols-alist
          (mapcar (lambda (abbrev-exp)
                    (let ((abbrev (car abbrev-exp))
                          (exp (cadr abbrev-exp)))
                      `(,exp . ,(vconcat (cdr (mapcan (lambda (ch) (list '(Br . Bl) ch)) abbrev))))))
                  lua-syntax-expansions))
    (prettify-symbols-mode 1)
    (dolist (abbrev-exp lua-syntax-expansions)
      (apply #'define-abbrev lua-mode-abbrev-table abbrev-exp))
    (modify-syntax-entry ?- "w 12")
    (abbrev-mode)
    (add-function :before (local 'abbrev-expand-function) #'lua-expand-abbrev-maybe)))

(use-package ob-lua :after org)

(use-package fennel-mode
  :vc (:url "git@git.sr.ht:~technomancy/fennel-mode" :branch "main" :rev :newest)
  :hook ((fennel-mode . fennel-proto-repl-minor-mode)
         ((fennel-mode
           fennel-repl-mode
           fennel-proto-repl-mode)
          . common-lisp-modes-mode))
  :bind ( :map fennel-mode-map
          ("M-." . xref-find-definitions)
          ("M-," . xref-go-back)
          :map fennel-repl-mode-map
          ("C-c C-o" . fennel-repl-delete-all-output))
  :custom
  (fennel-eldoc-fontify-markdown t)
  (fennel-scratch-use-proto-repl t)
  :preface
  (defun fennel-repl-delete-all-output ()
    (interactive)
    (save-excursion
      (goto-char (process-mark (get-buffer-process (current-buffer))))
      (forward-line 0)
      (let ((inhibit-read-only t))
        (delete-region (point) (point-min)))))
  :config
  (dolist (sym '(global local var set))
    (put sym 'fennel-indent-function 1)))

(use-package ob-fennel :after org)

(use-package clojure-mode
  :ensure t
  :hook ((clojure-mode
          clojurec-mode
          clojurescript-mode)
         . clojure-mode-setup)
  :commands (clojure-project-dir)
  :bind ( :map clojure-mode-map
          ("C-:" . nil))
  :preface
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
  :ensure t
  :delight " CIDER"
  :commands cider-find-and-clear-repl-buffer
  :functions (cider-nrepl-request:eval cider-find-and-clear-repl-output)
  :hook (((cider-repl-mode cider-mode) . eldoc-mode)
         (cider-repl-mode . common-lisp-modes-mode)
         (cider-popup-buffer-mode . cider-disable-linting))
  :bind ( :map cider-repl-mode-map
          ("C-c C-S-o" . cider-repl-clear-buffer)
          :map cider-mode-map
          ("C-c C-S-o" . cider-find-and-clear-repl-buffer)
          ("C-c C-p" . cider-pprint-eval-last-sexp-to-comment))
  :custom-face
  (cider-result-overlay-face ((t (:box (:line-width -1 :color "grey50")))))
  (cider-error-highlight-face ((t (:inherit flymake-error))))
  (cider-warning-highlight-face ((t (:inherit flymake-warning))))
  (cider-reader-conditional-face ((t (:inherit font-lock-comment-face))))
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
  (cider-auto-inspect-after-eval nil)
  (cider-comment-continued-prefix "")
  (cider-comment-prefix "")
  :config
  (put 'cider-clojure-cli-aliases 'safe-local-variable #'listp)
  (defun cider-disable-linting ()
    "Disable linting integrations for current buffer."
    (when (bound-and-true-p flymake-mode)
      (flymake-mode -1)))
  (defun cider-repl-prompt-newline (namespace)
    "Return a prompt string that mentions NAMESPACE with a newline."
    (format "%s\n> " namespace))
  (defun cider-find-and-clear-repl-buffer ()
    "Find the current REPL buffer and clear it.
See `cider-find-and-clear-repl-output' for more info."
    (interactive)
    (cider-find-and-clear-repl-output 'clear-repl))
  (defun cider-open-portal ()
    (interactive)
    (cider-nrepl-request:eval
     "(do
        (ns dev)
        (def portal ((requiring-resolve 'portal.api/open) {:launcher :emacs}))
        (add-tap (requiring-resolve 'portal.api/submit)))"
     #'ignore))
  (defun cider-clear-portal ()
    (interactive)
    (cider-nrepl-request:eval "(portal.api/clear)" #'ignore))
  (defun cider-close-portal ()
    (interactive)
    (cider-nrepl-request:eval "(portal.api/close)" #'ignore)))

(use-package ob-clojure
  :after (cider org)
  :custom
  (org-babel-clojure-backend 'cider))

(use-package clj-refactor
  :ensure t
  :delight clj-refactor-mode
  :hook ((clj-refactor-mode . yas-minor-mode)
         (cider-mode . clj-refactor-mode))
  :custom
  (cljr-suppress-no-project-warning t)
  (cljr-suppress-middleware-warnings t)
  (cljr-warn-on-eval nil))

(use-package clj-decompiler
  :ensure t
  :hook (cider-mode . clj-decompiler-setup))

(use-package lisp-mode
  :hook ((lisp-mode lisp-data-mode) . common-lisp-modes-mode))

(use-package inf-lisp
  :hook (inferior-lisp-mode . common-lisp-modes-mode)
  :bind ( :map common-lisp-modes-mode-map
          ("C-M-k" . lisp-eval-each-sexp))
  :commands (lisp-eval-last-sexp)
  :custom
  (inferior-lisp-program
   (cond ((executable-find "sbcl") "sbcl")
         ((executable-find "ecl") "ecl")))
  :config
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
  :ensure t
  :hook (sly-mrepl-mode . common-lisp-modes-mode)
  :commands (sly-symbol-completion-mode)
  :config
  (sly-symbol-completion-mode -1))

(use-package scheme
  :hook (scheme-mode . common-lisp-modes-mode))

(use-package geiser
  :ensure t
  :hook (scheme-mode . geiser-mode)
  :custom
  (geiser-active-implementations '(guile))
  (geiser-default-implementation 'guile))

(use-package geiser-guile
  :ensure t
  :after geiser)

(use-package sql-indent
  :ensure t)

;;; LSP

(use-package lsp-mode
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-completion-mode))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-diagnostics-provider :flymake)
  (lsp-completion-provider :none)
  (lsp-session-file (expand-file-name ".lsp-session" user-emacs-directory))
  (lsp-log-io nil)
  (lsp-keep-workspace-alive nil)
  (lsp-idle-delay 0.5)
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
  (lsp-completion-enable t)
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
  (lsp-signature-doc-lines 1)
  ;; lens
  (lsp-lens-enable nil)
  ;; semantic
  (lsp-semantic-tokens-enable nil)
  :init
  (setq lsp-use-plists t))

(use-package lsp-treemacs
  :ensure t
  :defer t
  :custom
  (lsp-treemacs-theme "Iconless"))

(use-package lsp-clojure
  :demand t
  :after lsp-mode
  :hook (cider-mode . cider-toggle-lsp-completion-maybe)
  :preface
  (defun cider-toggle-lsp-completion-maybe ()
    (lsp-completion-mode (if (bound-and-true-p cider-mode) -1 1))))

(use-package lsp-clojure
  :no-require
  :hook ((clojure-mode
          clojurec-mode
          clojurescript-mode)
         . lsp))

(use-package lsp-java
  :ensure t
  :demand t
  :after lsp-mode
  :when (and openjdk-11-path
             (file-exists-p openjdk-11-path))
  :custom
  (lsp-java-java-path openjdk-11-path))

(use-package lsp-java
  :no-require
  :hook (java-mode . lsp))

(use-package lsp-metals
  :ensure t
  :custom
  (lsp-metals-server-args
   '("-J-Dmetals.allow-multiline-string-formatting=off"))
  :hook (scala-mode . lsp))


;;; Navigation & Editing

(use-package common-lisp-modes
  :delight common-lisp-modes-mode
  :preface
  (defun indent-sexp-or-fill ()
    "Indent an s-expression or fill string/comment."
    (interactive)
    (let ((ppss (syntax-ppss)))
      (if (or (nth 3 ppss)
              (nth 4 ppss))
          (fill-paragraph)
        (save-excursion
          (mark-sexp)
          (indent-region (point) (mark))))))
  :bind ( :map common-lisp-modes-mode-map
          ("M-q" . indent-sexp-or-fill)))

(use-package puni
  :vc (:url "https://github.com/andreyorst/puni" :branch "fix-#49" :rev :newest)
  :hook (((common-lisp-modes-mode nxml-mode) . puni-mode)
         (puni-mode . electric-pair-mode))
  ;; paredit-like keys
  :bind ( :map puni-mode-map
          ("C-M-f" . puni-forward-sexp-or-up-list)
          ("C-M-b" . puni-backward-sexp-or-up-list)
          ("C-M-t" . puni-transpose)
          ;; slurping & barfing
          ("C-<left>" . puni-barf-forward)
          ("C-}" . puni-barf-forward)
          ("C-<right>" . puni-slurp-forward)
          ("C-)" . puni-slurp-forward)
          ("C-(" . puni-slurp-backward)
          ("C-M-<left>" . puni-slurp-backward)
          ("C-{" . puni-barf-backward)
          ("C-M-<right>" . puni-barf-backward)
          ;; depth chaning
          ("M-r" . puni-raise)
          ("M-s" . puni-splice)
          ("M-<up>" . puni-splice-killing-backward)
          ("M-<down>" . puni-splice-killing-forward)
          ("M-(" . puni-wrap-round)
          ("M-{" . puni-wrap-curly)
          ("M-?" . puni-convolute)
          ("M-S" . puni-split)
          :map region-bindings-mode-map
          ("(" . puni-wrap-round)
          ("[" . puni-wrap-square)
          ("{" . puni-wrap-curly)
          ("<" . puni-wrap-angle))
  :preface
  (define-advice puni-kill-line (:before (&rest _) back-to-indentation)
    "Go back to indentation before killing the line if it makes sense to."
    (when (looking-back "^[[:space:]]*" nil)
      (if (bound-and-true-p indent-line-function)
          (funcall indent-line-function)
        (back-to-indentation)))))

(use-package puni
  :when window-system
  :bind ( :map puni-mode-map
          ;; doesn't work in terminal
          ("M-[" . puni-wrap-square)))

(use-package isearch
  :bind ( :map isearch-mode-map
          ("<backspace>" . isearch-del-char)
          ("<left>" . isearch-edit-string)
          ("<right>" . isearch-edit-string)
          :map minibuffer-local-isearch-map
          ("<left>" . backward-char)
          ("<right>" . forward-char))
  :custom
  (isearch-lazy-highlight t))

(use-package dumb-jump
  :ensure t
  :defer t
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'completing-read)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package phi-search
  :ensure t
  :defer t)

(use-package avy
  :ensure t
  :bind
  (("C-:" . avy-goto-char-timer)
   ("C-M-:" . avy-goto-line)))

(use-package isayt
  :vc (:url "https://gitlab.com/andreyorst/isayt.el.git")
  :delight isayt-mode
  :hook (common-lisp-modes-mode . isayt-mode))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package region-bindings
  :vc (:url "https://gitlab.com/andreyorst/region-bindings.el.git")
  :preface
  (defun region-bindings-off ()
    (region-bindings-mode -1))
  :hook ((after-init . global-region-bindings-mode)
         ((elfeed-search-mode magit-mode mu4e-headers-mode)
          . region-bindings-off)))

(use-package multiple-cursors
  :ensure t
  :bind
  (("S-<mouse-1>" . mc/add-cursor-on-click)
   :map region-bindings-mode-map
   ("n" . mc/mark-next-symbol-like-this)
   ("N" . mc/mark-next-like-this)
   ("p" . mc/mark-previous-symbol-like-this)
   ("P" . mc/mark-previous-like-this)
   ("a" . mc/mark-all-symbols-like-this)
   ("A" . mc/mark-all-like-this)
   ("s" . mc/mark-all-in-region-regexp)
   ("l" . mc/edit-ends-of-lines)))

(use-package multiple-cursors-core
  :bind
  (( :map mc/keymap
     ("<return>" . nil)
     ("C-&" . mc/vertical-align-with-space)
     ("C-#" . mc/insert-numbers))))

(use-package vundo
  :ensure t
  :bind (("C-c u" . vundo))
  :custom
  (vundo-roll-back-on-quit nil)
  (vundo--window-max-height 10))

(use-package yasnippet
  :ensure t
  :defer t
  :delight yas-minor-mode)


;;; Tools

(use-package ediff
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (advice-add 'ediff-window-display-p :override #'ignore))

(use-package project
  :ensure t
  :bind ( :map project-prefix-map
          ("s" . project-save-some-buffers))
  :custom
  (project-compilation-buffer-name-function 'project-prefixed-buffer-name)
  (project-vc-extra-root-markers
   '("Cargo.toml" "compile_commands.json"
     "compile_flags.txt" "project.clj"
     "deps.edn" "shadow-cljs.edn"))
  :preface
  (defcustom project-compilation-mode nil
    "Mode to run the `compile' command with."
    :type 'symbol
    :group 'project
    :safe #'symbolp
    :local t)
  (defun project-save-some-buffers (&optional arg)
    "Save some modified file-visiting buffers in the current project.

Optional argument ARG (interactively, prefix argument) non-nil
means save all with no questions."
    (interactive "P")
    (let* ((project-buffers (project-buffers (project-current)))
           (pred (lambda () (memq (current-buffer) project-buffers))))
      (funcall-interactively #'save-some-buffers arg pred)))
  (define-advice compilation-start
      (:filter-args (args) use-project-compilation-mode)
    (let ((cmd (car args))
          (mode (cadr args))
          (rest (cddr args)))
      (if (and (null mode) project-compilation-mode)
          (append (list cmd project-compilation-mode) rest)
        args)))
  (define-advice project-root (:around (fn project) expand-project-root)
    (abbreviate-file-name (funcall fn project)))
  (defun project-make-predicate-buffer-in-project-p ()
    (let ((project-buffers (project-buffers (project-current))))
      (lambda () (memq (current-buffer) project-buffers))))
  (define-advice project-compile (:around (fn) save-project-buffers-only)
    "Only ask to save project-related buffers."
    (defvar compilation-save-buffers-predicate)
    (let ((compilation-save-buffers-predicate
           (project-make-predicate-buffer-in-project-p)))
      (funcall fn)))
  (define-advice recompile
      (:around (fn &optional edit-command) save-project-buffers-only)
    "Only ask to save project-related buffers if inside of a project."
    (defvar compilation-save-buffers-predicate)
    (let ((compilation-save-buffers-predicate
           (if (project-current)
               (project-make-predicate-buffer-in-project-p)
             compilation-save-buffers-predicate)))
      (funcall fn edit-command)))
  :config
  (add-to-list 'project-switch-commands
               '(project-dired "Dired"))
  (add-to-list 'project-switch-commands
               '(project-switch-to-buffer "Switch buffer")))

(use-package eat
  :ensure t
  :hook (eshell-load . eat-eshell-mode))

(use-package magit
  :ensure t
  :hook (git-commit-mode . flyspell-mode)
  :bind ( :map project-prefix-map
          ("m" . magit-project-status))
  :custom
  (magit-ediff-dwim-show-on-hunks t)
  (magit-diff-refine-ignore-whitespace t)
  (magit-diff-refine-hunk 'all)
  :config
  (define-advice magit-list-refnames
      (:filter-return (refs) range-at-point)
    (require 'thingatpt)
    (if-let ((range (save-match-data
                      (and (thing-at-point-looking-at "[a-f0-9]+\.\.[a-f0-9]+")
                           (match-string 0)))))
        (cons range refs)
      refs)))

(use-package magit
  :after project
  :config
  (add-to-list 'project-switch-commands
               '(magit-project-status "Magit") t))

(use-package server
  :commands (server-running-p)
  :init
  (unless (server-running-p)
    (server-start)))

(use-package separedit
  :ensure t
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
  (defun separedit-header-line-setup ()
    (setq-local
     header-line-format
     (substitute-command-keys
      "Edit, then exit with `\\[separedit-commit]' or abort with \\<edit-indirect-mode-map>`\\[edit-indirect-abort]'"))))

(use-package recentf
  :hook (after-init . recentf-mode)
  :defines (recentf-exclude)
  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 100)
  :config
  (add-to-list 'recentf-exclude "\\.gpg\\")
  (dolist (dir (list (expand-file-name ".cache/" user-emacs-directory)
                     (expand-file-name "workspace/.cache/" user-emacs-directory)))
    (add-to-list 'recentf-exclude (concat (regexp-quote dir) ".*"))))

(use-package gcmh
  :ensure t
  :hook (after-init . gcmh-mode)
  :delight gcmh-mode)

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode))

(use-package compile
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-scroll-output 'first-error)
  :commands (define-compilation-mode)
  :preface
  (cl-defun compile-add-error-syntax
      (mode name regexp &key file line col (level 'error))
    "Register new compilation error syntax.

Add NAME symbol to `compilation-error-regexp-alist', and then add
REGEXP FILE LINE and optional COL LEVEL info to
`compilation-error-regexp-alist-alist'."
    (or file (error "Missing value for :file keyword"))
    (or line (error "Missing value for :line keyword"))
    (let ((level (cond ((eq level 'info) 0)
                       ((eq level 'warn) 1)
                       ((eq level 'error) 2)
                       (t (error "Mnsupported level type: %S" level))))
          (mode (symbol-name (or mode 'compilation))))
      (add-to-list (intern (concat mode "-error-regexp-alist")) name)
      (add-to-list (intern (concat mode "-error-regexp-alist-alist"))
                   (list name regexp file line col level))))
  (defmacro define-project-compilation-mode (base-name &rest body)
    (declare (indent 1))
    (let* ((name (symbol-name base-name))
           (doc-name (capitalize (replace-regexp-in-string "-compilation$" "" name)))
           (current-project-root (intern (concat name "-current-project")))
           (current-project-files (intern (concat name "-current-project-files")))
           (compilation-mode-name (intern (concat name "-mode"))))
      `(progn
         (defvar ,(intern (concat name "-error-regexp-alist")) nil
           ,(concat "Alist that specifies how to match errors in " doc-name " compiler output.
See `compilation-error-regexp-alist' for more information."))
         (defvar ,(intern (concat name "-error-regexp-alist-alist")) nil
           ,(concat "Alist of values for `" (downcase doc-name) "-compilation-error-regexp-alist'.
See `compilation-error-regexp-alist-alist' for more information."))
         (defvar-local ,current-project-root nil
           ,(concat "Current root of the project being compiled.
Set automatically by the `" (symbol-name compilation-mode-name) "'."))
         (defvar-local ,current-project-files nil
           ,(concat "Current list of files belonging to the project being compiled.
Set automatically by the `" (symbol-name compilation-mode-name) "'."))
         (define-compilation-mode ,compilation-mode-name
           ,(concat doc-name " Compilation")
           ,(concat "Compilation mode for " doc-name " output.")
           (setq-local ,current-project-root (project-current t))
           (setq-local ,current-project-files (project-files ,current-project-root)))
         ,@body
         (provide ',compilation-mode-name)))))

(use-package clojure-compilation-mode
  :preface
  (define-project-compilation-mode clojure-compilation
    (defun clojure-compilation-filename-fn (rule-name)
      "Create a function that gets filename from the error message.

RULE-NAME is a symbol in `clojure-compilation-error-regexp-alist-alist'.
It is used to obtain the regular expression, which used for a
backward search in order to extract the filename from the first
group."
      (lambda ()
        "Get a filname from the error message and compute relative directory."
        (let* ((regexp (car (alist-get rule-name clojure-compilation-error-regexp-alist-alist)))
               (filename (save-match-data
                           (re-search-backward regexp)
                           (substring-no-properties (match-string 1)))))
          (if-let ((file (seq-find
                          (lambda (s)
                            (string-suffix-p filename s))
                          clojure-compilation-current-project-files)))
              (let* ((path-in-project
                      (substring file (length (project-root clojure-compilation-current-project))))
                     (dir (substring path-in-project 0 (- (length filename)))))
                (cons filename dir))
            filename)))))
  :config
  (compile-add-error-syntax
   'clojure-compilation
   'clj-kondo-warning
   "^\\(/[^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): warning"
   :file 1 :line 2 :col 3 :level 'warn)
  (compile-add-error-syntax
   'clojure-compilation
   'clj-kondo-error
   "^\\(/[^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): error"
   :file 1 :line 2 :col 3)
  (compile-add-error-syntax
   'clojure-compilation
   'kaocha-tap
   "^not ok.*(\\([^:]*\\):\\([0-9]*\\))$"
   :file (clojure-compilation-filename-fn 'kaocha-tap)
   :line 2)
  (compile-add-error-syntax
   'clojure-compilation
   'clojure-fail
   ".*\\(?:FAIL\\|ERROR\\) in.*(\\([^:]*\\):\\([0-9]*\\))$"
   :file (clojure-compilation-filename-fn 'clojure-fail)
   :line 2)
  (compile-add-error-syntax
   'clojure-compilation
   'clojure-reflection-warning
   "^Reflection warning,[[:space:]]*\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\).*$"
   :file (clojure-compilation-filename-fn 'clojure-reflection-warning)
   :line 2 :col 3
   :level 'warn)
  (compile-add-error-syntax
   'clojure-compilation
   'clojure-performance-warning
   "^Performance warning,[[:space:]]*\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\).*$"
   :file (clojure-compilation-filename-fn 'clojure-performance-warning)
   :line 2 :col 3
   :level 'warn)
  (compile-add-error-syntax
   'clojure-compilation
   'clojure-syntax-error
   "^Syntax error .* at (\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\))\.$"
   :file (clojure-compilation-filename-fn 'clojure-syntax-error)
   :line 2 :col 3)
  (compile-add-error-syntax
   'clojure-compilation
   'kaocha-unit-error
   "^ERROR in unit (\\([^:]+\\):\\([0-9]+\\))$"
   :file (clojure-compilation-filename-fn 'kaocha-unit-error)
   :line 2))

(use-package fennel-compilation-mode
  :preface
  (define-project-compilation-mode fennel-compilation)
  :config
  (compile-add-error-syntax
   'fennel-compilation
   'fennel-compile-error
   "^Compile error in \\(.*\.fnl\\):\\([[:digit:]]+\\):?\\([[:digit:]]+\\)?\\$"
   :file 1 :line 2 :col 3)
  (compile-add-error-syntax
   'fennel-compilation
   'fennel-compile-error-2
   "^\\(.*\.fnl\\):\\([[:digit:]]+\\):?\\([[:digit:]]+\\|\\?\\)? Compile error: "
   :file 1 :line 2 :col 3)
  (compile-add-error-syntax
   'fennel-compilation
   'fennel-test-error
   "^not ok[[:space:]]+[0-9]+[^
]+
#[[:space:]]+\\([^:]+\\):\\([0-9]+\\):"
   :file 1 :line 2 :level 'error)
  (compile-add-error-syntax
   'fennel-compilation
   'lua-stacktrace
   "\\(?:^[[:space:]]+\\([^
:]+\\):\\([[:digit:]]+\\):[[:space:]]+in.+$\\)"
   :file 1 :line 2))

(use-package zig-compilation-mode
  :preface
  (define-project-compilation-mode zig-compilation)
  :config
  (compile-add-error-syntax
   'zig-compilation
   'zig-error
   "^\\(/[^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): error:"
   :file 1 :line 2 :col 3)
  (compile-add-error-syntax
   'zig-compilation
   'zig-note
   "^\\(/[^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): note:"
   :file 1 :line 2 :col 3 :level 'info)
  (compile-add-error-syntax
   'zig-compilation
   'zig-error-in
   "^\\(/[^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):.*in.*([^)]+)"
   :file 1 :line 2 :col 3 :level 'info)
  (compile-add-error-syntax
   'zig-compilation
   'zig-reference
   "^[[:space:]]+\\(?:freeObject\\|deinit\\):[[:space:]]\\(/[^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)"
   :file 1 :line 2 :col 3 :level 'info))

(use-package password-store
  :when (executable-find "pass")
  :commands (password-store-copy
             password-store-get
             password-store-insert
             password-store-generate)
  :functions (password-store--completing-read@use-orderless)
  :load-path "/usr/share/doc/pass/emacs/"
  :config
  (define-advice password-store--completing-read
      (:around (fn &optional require-match) use-orderless)
    (let ((completion-styles (append completion-styles '(orderless))))
      (funcall fn require-match))))

(use-package jdecomp
  :ensure t
  :mode ("\\.class\\'" . jdecomp-mode)
  :preface
  (defvar cfr-path
    (file-truename "~/.local/lib/cfr.jar")
    "Path to the cfr Java decompiler library.")
  (defvar fernflower-path
    (file-truename "~/.local/lib/fernflower.jar")
    "Path to the FernFlower library.")
  :when (or (file-exists-p cfr-path)
            (file-exists-p fernflower-path))
  :custom
  (jdecomp-decompiler-type
   (cond ((file-exists-p cfr-path) 'cfr)
         ((file-exists-p fernflower-path) 'fernflower)))
  (jdecomp-decompiler-paths
   `((cfr . ,cfr-path)
     (fernflower . ,fernflower-path))))

(use-package gsettings
  :ensure t)

(use-package proxy
  :after gsettings
  :preface
  (defun proxy--get-host (type)
    (let ((host (gsettings-get (format "org.gnome.system.proxy.%s" type) "host")))
      (and (not (string-empty-p host)) host)))
  (defun proxy--get-port (type)
    (let ((port (gsettings-get (format "org.gnome.system.proxy.%s" type) "port")))
      (and (numberp port) port)))
  (defun proxy--get-ignore ()
    (let ((ignore (gsettings-get "org.gnome.system.proxy" "ignore-hosts")))
      (and (vectorp ignore) (string-join ignore ","))))
  (defun proxy--enabled? ()
    (string= "manual" (gsettings-get "org.gnome.system.proxy" "mode")))
  (defun proxy-setup-env ()
    "Aqcuire proxy information from the settings and set the ENV vars."
    (interactive)
    (let ((vars '("http_proxy" "https_proxy" "ftp_proxy" "no_proxy")))
      (if-let ((enabled (proxy--enabled?))
               (host (proxy--get-host "http"))
               (port (proxy--get-port "http"))
               (ignore (proxy--get-ignore))
               (proxy (format "http://%s:%s" host port)))
          (dolist (v (append vars (mapcar #'upcase vars)))
            (setenv v (pcase (downcase v) ("no_proxy" ignore) (_ proxy))))
        (dolist (v (append vars (mapcar #'upcase vars)))
          (setenv v)))))
  (provide 'proxy)
  :init
  (proxy-setup-env))


;;; Messaging

(use-package erc
  :defer t
  :custom
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  (erc-fill-column 110)
  :functions (erc-update-modules)
  :autoload (erc-compute-server)
  :preface
  (defcustom erc-tunnel-conf nil
    "Connection spec for IRC server behind an SSH tunnel.
Can be used to connect to a bouncer running behind SSH.  A plist
with the following keys:

  :host - remote host.

  :port - remote port.

  :ssh-port - SSH port.
              Optional, needed when SSH is running on a custom
              port.

The rest options for the tunnel are taken from ERC directly.  See
the options: `erc-port' or `erc-default-port', and `erc-server'."
    :tag "ERC tunnel configuration"
    :type 'plist
    :group 'erc)
  (defcustom erc-pass ""
    "Password to pass to the `erc' function."
    :tag "ERC password for the bouncer"
    :type 'string
    :group 'erc)
  (defun erc-setup-port-forwarding (conf)
    "Setup a connection function based on plist CONF.
Connection is specified by the keys :host, :port, and an optional
:ssh-port. Returns a lambda which will call `start-process' with
the generated command."
    (when-let ((host (plist-get conf :host))
               (port (plist-get conf :port)))
      (let* ((name "erc-tunnel")
             (buf (format " *%s-%s:%s*" name host port))
             (cmd (format "ssh -L %s:%s:%s %s %s"
                          (or erc-port erc-default-port)
                          (or erc-server "localhost")
                          port
                          (if-let ((ssh-port (plist-get conf :ssh-port)))
                              (format "-p %s" ssh-port)
                            "")
                          host)))
        (lambda (&rest _)
          (when (or (not (get-buffer buf))
                    (not (get-buffer-process (get-buffer buf))))
            (apply #'start-process name buf (split-string-shell-command cmd)))))))
  (defun erc-connect ()
    "Connect to the bouncer."
    (interactive)
    (erc :server (or erc-server (erc-compute-server))
         :port (or erc-port erc-default-port)
         :nick erc-nick
         :password erc-pass))
  :config
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'spelling)
  (erc-update-modules)
  (when-let ((hook (erc-setup-port-forwarding erc-tunnel-conf)))
    (add-hook 'erc-connect-pre-hook hook)))

(use-package message
  :defer t
  :custom
  (message-kill-buffer-on-exit t))

(use-package message-view-patch
  :ensure t
  :hook (gnus-part-display . message-view-patch-highlight))

(use-package smtpmail
  :defer t)

(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :when (executable-find "mu")
  :defines mu4e-personal-addresses
  :commands (mu4e mu4e-ask-maildir make-mu4e-context mu4e-context-current mu4e-get-maildirs)
  :functions (mu4e-message-field mu4e-context-vars)
  :requires seq
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
  :preface
  (defvar mail-contexts nil
    "A list of plists with context related settings.

     Each plist contains the following keywords:

     - :inbox - directory for the mbsync inbox;
     - :name - name of the inbox for mu4e to use as context name;
     - :smtp-name - account email;
     - :smtp-server - smtp server;
     - :port - smtp server port;
     - :address - account email;
     - :signature - signature to use when composing emails.")
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
               (mu4e-compose-signature . ,(or (plist-get ctx :signature) user-full-name))
               (mu4e-maildir-context . ,inbox)

               (smtpmail-smtp-user . ,(plist-get ctx :smtp-name))
               (smtpmail-local-domain . ,(plist-get ctx :smtp-server))
               (smtpmail-smtp-server . ,(plist-get ctx :smtp-server))
               (smtpmail-smtp-service . ,(plist-get ctx :port))

               (user-mail-address . ,(plist-get ctx :address))
               (send-mail-function . smtpmail-send-it)))))
  (define-advice mu4e-get-maildirs (:around (fn) filter-maildirs-based-on-context)
    "Filters maildirs for current active context based on maildir prefix."
    (let* ((context-vars (mu4e-context-vars (mu4e-context-current)))
           (current-maildir (alist-get 'mu4e-maildir-context context-vars)))
      (if current-maildir
          (seq-filter (lambda (maildir)
                        (string-prefix-p current-maildir maildir))
                      (funcall fn))
        (funcall fn))))
  :config
  (defvar mu4e-contexts)
  (when (load (expand-file-name "mail-contexts.el" user-emacs-directory) 'noerror)
    (setq mu4e-contexts (mapcar #'make-context mail-contexts)
          user-mail-address (plist-get (car mail-contexts) :address)
          mu4e-personal-addresses (mapcar (lambda (ctx) (plist-get ctx :address))
                                          mail-contexts))))

(use-package mu4e
  :no-require
  :after (orderless mu4e)
  :functions (mu4e-ask-maildir@use-orderless)
  :config
  (define-advice mu4e-ask-maildir (:around (fn prompt) use-orderless)
    (let ((completion-styles (append completion-styles '(orderless))))
      (funcall fn prompt))))

(use-package mu4e-alert
  :ensure t
  :after mu4e
  :custom
  (mu4e-alert-style 'libnotify)
  (mu4e-alert-icon "emacs")
  :config
  (mu4e-alert-enable-notifications))

(use-package elfeed
  :ensure t)

(provide 'init)
;;; init.el ends here
