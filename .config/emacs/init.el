;;; init.el --- Main configuration file -*- lexical-binding: t; no-byte-compile: t -*-

;; Author: Andrey Listopadov
;; Keywords: Emacs configuration
;; Homepage: https://gitlab.com/andreyorst/dotfiles.git

;;; Commentary:
;; Emacs 30+ configuration.

;;; Code:

(use-package use-package
  :no-require
  :custom
  (use-package-enable-imenu-support t))

(use-package early-init
  :no-require
  :unless (featurep 'early-init)
  :config
  (load-file (locate-user-emacs-file "early-init.el")))

(use-package delight
  :ensure t)

(use-package local-config
  :no-require
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
  (defcustom no-hscroll-modes '(term-mode)
    "Major modes to disable horizontal scrolling."
    :tag "Modes to disable horizontal scrolling"
    :type '(repeat symbol)
    :group 'local-config)
  (provide 'local-config))

(use-package functions
  :no-require
  :functions (dbus-color-theme-dark-p)
  :bind (("M-Q" . split-pararagraph-into-lines))
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
  (defun dark-mode-enabled-p ()
    "Check if dark mode is enabled."
    (cond ((file-exists-p (expand-file-name "~/.dark-mode")) t)
          ((featurep 'dbus) (dbus-color-theme-dark-p))
          (t nil)))
  (defun memoize (fn)
    "Create a storage for FN's args.
Checks if FN was called with set args before.  If so, return the
value from the storage and don't call FN.  Otherwise calls FN,
and saves its result in the storage.  FN must be referentially
transparent."
    (let ((memo (make-hash-table :test 'equal)))
      (lambda (&rest args)
        ;; `memo' is used as a singleton to check for absense of value
        (let ((value (gethash args memo memo)))
          (if (eq value memo)
              (puthash args (apply fn args) memo)
            value)))))
  (defmacro defmemo (name &rest funtail)
    "Define memoized function NAME.
FUNTAIL is the rest of arguments to a regular `defun' after the
function's name."
    (declare (doc-string 3) (indent 2) (debug defun))
    `(defalias ',name (memoize (lambda ,@funtail))))
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(defmemo\\)\\_>\\s *\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t))))
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
  :no-require
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
     cursor-in-non-selected-windows nil))
  (setq
   ring-bell-function 'ignore
   mode-line-percent-position nil
   enable-recursive-minibuffers t)
  (when (version<= "27.1" emacs-version)
    (setq bidi-inhibit-bpa t))
  (provide 'defaults))


;;; Core packages

(use-package window
  :config
  (add-to-list
   'display-buffer-alist
   '("\\*Calendar*" (display-buffer-at-bottom))))

(use-package mouse
  :bind (("<mode-line> <mouse-2>" . nil)
         ("<mode-line> <mouse-3>" . nil)))

(use-package mode-line
  :no-require
  :preface
  (defvar mode-line-interactive-position
    `(line-number-mode
      (:propertize "%l:%C"
                   help-echo "mouse-1: Goto line"
                   mouse-face mode-line-highlight
                   local-map ,(let ((map (make-sparse-keymap)))
                                (define-key map [mode-line down-mouse-1] 'goto-line)
                                map)))
    "Mode line position with goto line binding.")
  (put 'mode-line-interactive-position 'risky-local-variable t)
  (fset 'abbreviate-file-name-memo (memoize #'abbreviate-file-name))
  (defvar mode-line-buffer-file-name
    '(:eval (propertize (if-let* ((name (buffer-file-name)))
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
    '(:eval (propertize (or current-input-method-title "EN")
                        'help-echo (concat "Input method: " (or current-input-method "default"))))
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
    '(:eval (when-let* ((eol (pcase (coding-system-eol-type buffer-file-coding-system)
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
                '(" " mode-line-buffer-file-name " " mode-line-modes
                  mode-line-format-right-align mode-line-misc-info
                  mode-line-input-method mode-line-buffer-encoding
                  mode-line-line-encoding " " mode-line-interactive-position
                  (vc-mode vc-mode) "  "))
  (provide 'mode-line))

(use-package font
  :no-require
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
  (custom-file (locate-user-emacs-file "custom.el"))
  :init
  (load custom-file :noerror))

(use-package novice
  :preface
  (defvar disabled-commands (locate-user-emacs-file "disabled.el")
    "File to store disabled commands, that were enabled permanently.")
  (define-advice enable-command (:around (fn command) use-disabled-file)
    (let ((user-init-file disabled-commands))
      (funcall fn command)))
  :init
  (load disabled-commands 'noerror))

(use-package files
  :preface
  (defvar backup-dir
    (locate-user-emacs-file ".cache/backups")
    "Directory to store backups.")
  (defvar auto-save-dir
    (locate-user-emacs-file ".cache/auto-save/")
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
  (defun window-font-width-unscaled ()
    (let (face-remapping-alist)
      (window-font-width)))
  (defun truncated-lines-p ()
    "Non-nil if any line is longer than `window-width' + `window-hscroll'.

Returns t if any line exceeds the right border of the window.
Used for stopping scroll from going beyond the longest line.
Based on `so-long-detected-long-line-p'."
    (let ((buffer (current-buffer))
          (tabwidth tab-width)
          (start (window-start))
          (end (window-end)))
      (let* ((window-width
              ;; this computes a more accurate width rather than `window-width', and
              ;; respects `text-scale-mode' font width.
              (/ (window-body-width nil t) (window-font-width)))
             (hscroll-offset
              ;; `window-hscroll' returns columns that are not affected by
              ;; `text-scale-mode'.  Because of that, we have to recompute the correct
              ;; `window-hscroll' by multiplying it with a non-scaled value and
              ;; dividing it with a scaled width value, rounding it to the upper
              ;; boundary.
              (ceiling (/ (* (window-hscroll) (window-font-width-unscaled))
                          (float (window-font-width)))))
             (line-number-width
              ;; compensate line numbers width
              (if (bound-and-true-p display-line-numbers-mode)
                  (- display-line-numbers-width)
                0))
             (threshold (+ window-width hscroll-offset line-number-width
                           -2)))   ; compensate imprecise calculations
        (with-temp-buffer
          (insert-buffer-substring buffer start end)
          (let ((tab-width tabwidth))
            (untabify (point-min) (point-max)))
          (goto-char (point-min))
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
                  (throw 'excessive t)))))))))
  (define-advice scroll-left (:before-while (&rest _) prevent-overscroll)
    (and truncate-lines
         (not (memq major-mode no-hscroll-modes))
         (truncated-lines-p)))
  :init
  (if (fboundp #'context-menu-mode)
      (context-menu-mode 1)
    (global-set-key (kbd "<mouse-3>") menu-bar-edit-menu))
  (unless (display-graphic-p)
    (xterm-mouse-mode t)))

(use-package face-remap
  :bind ([remap text-scale-pinch] . ignore))

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
         ("C-x k" . kill-current-buffer)
         ("C-h C-f" . describe-face)
         ([remap undo] . undo-only))
  :hook ((before-save . delete-trailing-whitespace)
         (overwrite-mode . overwrite-mode-set-cursor-shape)
         (after-init . column-number-mode)
         (after-init . line-number-mode))
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
        (funcall-interactively quit)))))

(use-package delsel
  :hook (after-init . delete-selection-mode))

(use-package common-lisp-modes
  :vc ( :url "https://gitlab.com/andreyorst/common-lisp-modes.el.git"
        :branch "main"
        :rev :newest))

(use-package minibuffer
  :hook (eval-expression-minibuffer-setup . common-lisp-modes-mode)
  :bind ( :map minibuffer-inactive-mode-map
          ("<mouse-1>" . ignore))
  :custom
  (completion-styles '(partial-completion basic))
  (read-buffer-completion-ignore-case t)
  ;; (read-file-name-completion-ignore-case t)
  :custom-face
  (completions-first-difference ((t (:inherit unspecified)))))

(use-package bindings
  :bind ( :map ctl-x-map
          ("DEL" . nil)
          ("C-d" . dired-jump))
  :init
  (setq mode-line-end-spaces nil))

(use-package frame
  :requires seq
  :bind (("C-z" . ignore)
         ("C-x C-z" . ignore)))

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
  (tooltip-y-offset (line-pixel-height))
  (tooltip-frame-parameters
   `((name . "tooltip")
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
    (equal '1 (caar (dbus-ignore-errors
                      (dbus-call-method
                       :session
                       "org.freedesktop.portal.Desktop"
                       "/org/freedesktop/portal/desktop"
                       "org.freedesktop.portal.Settings"
                       "Read"
                       "org.freedesktop.appearance"
                       "color-scheme")))))
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
   'no-confirm))

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
   (remove 'eshell-term eshell-modules-list)))

(use-package dired
  :bind ( :map dired-mode-map
          ("<backspace>" . dired-up-directory)
          ("M-<up>" . dired-up-directory)
          ("~" . dired-home-directory)
          ("C-c l" . org-store-link)
          ("h" . dired-toggle-dotfiles))
  :functions (dired-current-directory)
  :hook (dired-mode . dired-hide-details-mode)
  :preface
  (defvar dired-listing-switches-no-dotfiles
    "-lXhv --group-directories-first")
  (defvar dired-listing-switches-dotfiles
    "-lAXhv --group-directories-first")
  :custom
  (dired-listing-switches dired-listing-switches-dotfiles)
  :config
  (defun dired-home-directory ()
    (interactive)
    (dired (expand-file-name "~/")))
  (defun dired-toggle-dotfiles ()
    (interactive)
    (setf dired-listing-switches
          (if (string= dired-listing-switches
                       dired-listing-switches-no-dotfiles)
              dired-listing-switches-dotfiles
            dired-listing-switches-no-dotfiles))
    (dolist (buffer dired-buffers)
      (with-current-buffer (cdr buffer)
        (dired-noselect (dired-current-directory) dired-listing-switches)))))

(use-package comint
  :defer t
  :custom
  (comint-scroll-show-maximum-output nil)
  (comint-highlight-input nil)
  (comint-input-ignoredups t))

(use-package rect
  :bind (("C-x r C-y" . rectangle-yank-add-lines))
  :custom
  (rectangle-indicate-zero-width-rectangle nil)
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
  :preface
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
  :config
  (setq elisp-flymake-byte-compile-load-path (cons "./" load-path)))

(use-package flyspell
  :ensure t
  :when (or (executable-find "ispell")
            (executable-find "aspell")
            (executable-find "hunspell"))
  :hook ((org-mode git-commit-mode markdown-mode) . flyspell-mode))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

(use-package outline
  :hook ((common-lisp-modes-mode . lisp-outline-minor-mode))
  :delight outline-minor-mode
  :custom
  (outline-minor-mode-cycle t)
  :preface
  (defun lisp-outline-minor-mode ()
    (setq-local outline-regexp "^;;;;*[[:space:]]\\w")
    (outline-minor-mode)))

(use-package browse-url
  :custom (browse-url-browser-function
           (if (featurep 'xwidget-internal)
               #'xwidget-webkit-browse-url
             #'eww-browse-url)))

(use-package repeat
  :hook (after-init . repeat-mode))

(use-package indirect-narrow
  :bind ( :map narrow-map
          ("i n" . indirect-narrow-to-region)
          ("i d" . indirect-narrow-to-defun)
          ("i p" . indirect-narrow-to-page))
  :preface
  (defun indirect-narrow-to-region (start end)
    (interactive "r")
    (deactivate-mark)
    (with-current-buffer (clone-indirect-buffer nil nil)
      (narrow-to-region start end)
      (pop-to-buffer (current-buffer))))
  (defun indirect-narrow-to-page (&optional arg)
    (interactive "P")
    (deactivate-mark)
    (with-current-buffer (clone-indirect-buffer nil nil)
      (narrow-to-page arg)
      (pop-to-buffer (current-buffer))))
  (defun indirect-narrow-to-defun (&optional include-comments)
    (interactive (list narrow-to-defun-include-comments))
    (deactivate-mark)
    (with-current-buffer (clone-indirect-buffer nil nil)
      (narrow-to-defun include-comments)
      (pop-to-buffer (current-buffer))))
  (provide 'indirect-narrow))

(use-package page
  ;; I often input C-x C-p instead of C-x p followed by project
  ;; key, deleting contents of whole buffer as a result.
  :bind ("C-x C-p" . nil))


;;; Completion

(use-package vertico
  :ensure t
  :bind ( :map vertico-map
          ("M-RET" . vertico-exit-input))
  :hook (after-init . vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind ( :map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package consult
  :ensure t
  :commands (consult-completion-in-region)
  :preface
  (defvar consult-prefix-map (make-sparse-keymap))
  (fset 'consult-prefix-map consult-prefix-map)
  :bind ( :map ctl-x-map
          ("c" . consult-prefix-map)
          :map consult-prefix-map
          ("r" . consult-recent-file))
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
  :commands (corfu-quit)
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
  :hook after-init)

(use-package cape
  :ensure t
  :after corfu
  :config
  (setq completion-at-point-functions '(cape-file)))

(use-package ov
  :ensure t
  :commands (ov-regexp))


;;; Org

(use-package org
  :hook ((org-babel-after-execute . org-redisplay-inline-images))
  :bind ( :map org-mode-map
          ("C-c l" . org-store-link))
  :custom-face
  (org-block ((t (:extend t))))
  (org-block-begin-line
   ((t ( :slant unspecified
         :weight normal
         :background unspecified
         :inherit (org-block shadow)
         :extend t))))
  (org-block-end-line
   ((t ( :slant unspecified
         :weight normal
         :background unspecified
         :inherit org-block-begin-line
         :extend t))))
  (org-drawer ((t (:foreground unspecified :inherit shadow))))
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
  (org-src-preserve-indentation t)
  :config
  (defun org-babel-edit-prep:emacs-lisp (_)
    "Setup Emacs Lisp buffer for Org Babel."
    (setq lexical-binding t))
  (unless (version<= org-version "9.1.9")
    (add-to-list 'org-modules 'org-tempo)))

(use-package ob-shell :after org)

(use-package org-capture
  :bind ( :map mode-specific-map
          ("o c" . org-capture)))

(use-package blog
  :after org-capture
  :vc ( :url "https://gitlab.com/andreyorst/blog.el.git"
        :branch "main"
        :rev :newest))

(use-package blog
  :after blog
  :no-require
  :custom
  (org-directory blog-directory)
  (org-capture-templates
   `(,blog-capture-template
     ,@(mapcar
        (lambda (spec)
          (seq-let (btn descr heading) spec
            `( ,btn ,descr entry
               (file+headline ,(expand-file-name "notes/index.org" blog-directory) ,heading)
               "* [[blog-html:%^{Link}][%^{Description}]]\n:properties:\n:blog-collapsable: t\n:end:"
               :immediate-finish t
               :before-finalize (org-hugo-export-to-md))))
        '(("a" "Article" "Articles")
          ("t" "Talk" "Talks")
          ("w" "Web page" "Various Web pages")
          ("b" "Books, courses" "Books, Courses"))))))

(use-package ol
  :after org-capture
  :functions (org-link-set-parameters)
  :preface
  (defun blog-follow-html-link (path arg)
    (funcall browse-url-browser-function path arg))
  (defun blog-export-hmtl-link (path description _backend _properties)
    "Export link directly to HTML."
    (format "<a href=\"%s\">%s</a>" path (or description path)))
  (defun blog-create-html-link (&optional _)
    "Create a file link using completion."
    (let ((link (read-string "Link: ")))
      (concat "blog-html:" link)))
  :config
  (org-link-set-parameters
   "blog-html"
   :follow #'blog-follow-html-link
   :export #'blog-export-hmtl-link
   :complete #'blog-create-html-link))

(use-package blog
  :after ol
  :functions
  (blog-export-static-org-link
   blog-create-static-org-link)
  :config
  (org-link-set-parameters
   "org"
   :export #'blog-export-static-org-link
   :complete #'blog-create-static-org-link))

(use-package ox-hugo
  :ensure t
  :after ox
  :preface
  (declare-function org-export-data "ext:ox")
  (declare-function org-export-get-node-property "ext:ox")
  (declare-function org-element-property "ext:org-element-ast")
  (declare-function org-element--property "ext:org-element-ast")
  (define-advice org-hugo-heading (:around (fn heading contents info) patch)
    (if (and (org-export-get-node-property :BLOG-COLLAPSABLE heading) (not (string-empty-p contents)))
        (let ((title (org-export-data (org-element-property :title heading) info)))
          (concat "<details class=\"foldlist\"><summary>" title
                  "</summary><div class=\"foldlistdata\">\n\n"
                  contents
                  "</div></details>"))
      (funcall fn heading contents info))))

(use-package ox-latex
  :after ox)

(use-package org-modern
  :ensure t
  :defer t
  :custom
  (org-modern-block-fringe nil)
  (org-modern-block-fringe nil)
  (org-modern-fold-stars nil)
  (org-modern-hide-stars nil)
  (org-modern-star nil)
  (org-modern-list nil))

(use-package epresent
  :ensure t
  :custom
  (epresent-text-scale 144)
  (epresent-format-latex-scale 1.44)
  :hook
  (epresent-start-presentation . epresent-setup)
  :preface
  (defcustom epresent-margin-size 400
    "A margin size to center everything on screen."
    :type 'number
    :group 'epresent)
  (declare-function org-modern-mode "ext:org-modern")
  (declare-function modus-themes-get-color-value "ext:modus-themes")
  (defun epresent-setup ()
    (interactive)
    (when (require 'org-modern nil 'noerror)
      (org-modern-mode 1))
    (visual-line-mode 1)
    (flyspell-mode -1)
    (set-window-fringes
     (selected-window)
     epresent-margin-size
     epresent-margin-size)
    (when (require 'modus-themes nil 'noerror)
      (set-face-attribute
       'org-block (selected-frame)
       :background (modus-themes-get-color-value 'bg-dim)))
    (set-face-attribute
     'header-line (selected-frame)
     :height (* epresent-margin-size 2)
     :background 'unspecified)
    (setq-local header-line-format " ")))


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

(use-package yaml-mode
  :ensure t
  :defer t
  :custom
  (yaml-indent-offset 4))

(use-package css-mode
  :defer t
  :custom
  (css-indent-offset 2))

(use-package js
  :defer t
  :custom
  (js-indent-level 2))

(use-package csv-mode
  :ensure t
  :hook ((csv-mode . csv-guess-set-separator))
  :custom
  (csv-align-max-width most-positive-fixnum))

(use-package scala-mode
  :ensure t
  :defer t)

(use-package zig-mode
  :ensure t
  :defer t)

(use-package abbrev
  :delight abbrev-mode
  :custom
  (save-abbrevs nil))

(use-package ob-lua :after org)

(use-package gnuplot
  :ensure t
  :when (executable-find "gnuplot"))

(use-package ob-gnuplot
  :when (executable-find "gnuplot")
  :after org)

(use-package fennel-mode
  :mode "\\.fnlm"
  :vc ( :url "https://git.sr.ht/~technomancy/fennel-mode"
        :branch "main"
        :rev :newest)
  :hook ((fennel-mode . fennel-proto-repl-minor-mode)
         (fennel-mode . fennel-mode-set-compile-command)
         ((fennel-mode
           fennel-repl-mode
           fennel-proto-repl-mode)
          . common-lisp-modes-mode))
  :bind ( :map fennel-repl-mode-map
          ("C-c C-o" . fennel-repl-delete-all-output))
  :custom
  (fennel-eldoc-fontify-markdown t)
  (fennel-scratch-use-proto-repl t)
  :config
  (put 'fennel-program 'safe-local-variable
       (lambda (s) (string-match-p "^\\(fennel\\|love\\)" s)))
  (defun fennel-repl-delete-all-output ()
    (interactive)
    (save-excursion
      (goto-char (process-mark (get-buffer-process (current-buffer))))
      (forward-line 0)
      (let ((inhibit-read-only t))
        (delete-region (point) (point-min)))))
  (defun fennel-mode-set-compile-command ()
    (setq-local
     compile-command
     (if (executable-find "deps")
         "deps --profiles dev "
       "fennel "))))

(use-package fennel-proto-repl
  :after fennel-mode
  :custom
  (fennel-proto-repl-project-integration 'project)
  (fennel-proto-repl-font-lock-dynamically '(scoped-macro global)))

(use-package ob-fennel :after org)

(use-package fennel-ls-flymake
  :after fennel-mode
  :hook (fennel-mode . fennel-ls-flymake))

(use-package fennel-extras
  :after fennel-mode
  :preface
  (dolist (sym '(require-macros))
    (put sym 'fennel-indent-function 'defun))
  (dolist (sym '( tset global local var set catch
                  import-macros pick-values
                  testing deftest use-fixtures go-loop))
    (put sym 'fennel-indent-function 1))
  (dolist (sym '(go))
    (put sym 'fennel-indent-function 0))
  (font-lock-add-keywords
   'fennel-mode
   `((,(rx (syntax open-parenthesis)
           word-start (or "defn" "defn-" "def" "deftest") word-end (1+ space)
           (group (1+ (or (syntax word) (syntax symbol) "-" "_"))))
      1 font-lock-function-name-face)))
  (provide 'fennel-extras))

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
  :after clojure-mode
  :delight " CIDER"
  :commands cider-find-and-clear-repl-buffer
  :functions (cider-nrepl-request:eval
              cider-find-and-clear-repl-output
              cider-random-tip)
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
  (cider-repl-history-file (expand-file-name "~/.cider-history"))
  (cider-clojure-cli-global-options "-J-XX:-OmitStackTraceInFastThrow")
  (cider-use-tooltips nil)
  (cider-connection-message-fn #'cider-random-tip)
  (cider-repl-prompt-function #'cider-repl-prompt-newline)
  (cider-auto-inspect-after-eval nil)
  (cider-enrich-classpath nil) ; causes troubles behind proxy and with add-lib feature
  (cider-download-java-sources t)
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
    (cider-find-and-clear-repl-output 'clear-repl)))

(use-package ob-clojure
  :after (org clojure-mode)
  :custom
  (org-babel-clojure-backend 'cider)
  :init
  (require 'cider))

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

(use-package sql-indent
  :defer t
  :ensure t)

(use-package sql-clickhouse
  :after sql
  :ensure t)


;;;; tree-sitter modes

(use-package treesit
  :when (treesit-p)
  :functions (treesit-ready-p)
  :preface
  (defun treesit-p ()
    "Check if Emacs was built with treesiter in a protable way."
    (and (fboundp 'treesit-available-p)
         (treesit-available-p)))
  (cl-defun treesit-install-and-remap
      (lang url &key revision source-dir modes remap org-src)
    "Convenience function for installing and enabling a ts-* mode.

LANG is the language symbol.  URL is the Git repository URL for the
grammar.  REVISION is the Git tag or branch of the desired version,
defaulting to the latest default branch.  SOURCE-DIR is the relative
subdirectory in the repository in which the grammar’s parser.c file
resides, defaulting to \"src\".  MODES is a list of modes to remap to a
symbol REMAP.  ORG-SRC is a cons specifying a source code block language
name and a corresponding major mode."
    (when (and (fboundp 'treesit-available-p)
               (treesit-available-p))
      (unless (treesit-language-available-p lang)
        (add-to-list
         'treesit-language-source-alist
         (list lang url revision source-dir))
        (treesit-install-language-grammar lang))
      (when (and remap (treesit-ready-p lang))
        (dolist (mode modes)
          (add-to-list
           'major-mode-remap-alist
           (cons mode remap))))
      (when (and org-src (treesit-ready-p lang))
        (eval-after-load 'org
          (lambda ()
            (add-to-list 'org-src-lang-modes org-src))))))
  :custom
  (treesit-font-lock-level 2))

(use-package js
  :defer t
  :when (treesit-p)
  :init
  (treesit-install-and-remap
   'javascript "https://github.com/tree-sitter/tree-sitter-javascript"
   :revision "master" :source-dir "src"
   :modes '(js-mode javascript-mode js2-mode)
   :remap 'js-ts-mode
   :org-src '("js" . js-ts)))

(use-package json-ts-mode
  :defer t
  :after json
  :when (treesit-p)
  :init
  (treesit-install-and-remap
   'json "https://github.com/tree-sitter/tree-sitter-json"
   :modes '(js-json-mode)
   :remap 'json-ts-mode
   :org-src '("json" . json-ts)))

(use-package json-hs-extra
  :after json
  :hook (json-ts-mode . json-hs-extra-setup)
  :preface
  (defun json-hs-extra-create-overlays (overlay)
    "Creates overlays for block beginning, hiding whitespace.
Sets OVERLAY `json-hs-extra-overlays' property to the list of created
overlays."
    (let ((end (point)))
      (save-excursion
        (forward-sexp -1)
        (when-let* ((overlays (ov-regexp "{[[:space:]\n]*" (point) end)))
          (mapc (lambda (ov) (overlay-put ov 'display "{")) overlays)
          (overlay-put overlay 'json-hs-extra-overlays overlays)))))
  (defun json-hs-extra-delete-overlays (fn overlay)
    "Deletes overlays for block beginning created earlier.
Deletes overlays in the `json-hs-extra-overlays' property of OVERLAY,
created with `json-hs-extra-create-overlays'."
    (mapc #'delete-overlay (overlay-get overlay 'json-hs-extra-overlays))
    (funcall fn overlay))
  (defun json-hs-extra-setup ()
    "Special settings for JSON buffers."
    (setq-local hs-block-start-regexp "\\(?:{[[:space:]\n]*\\|\\[\\)"
                hs-set-up-overlay #'json-hs-extra-create-overlays))
  (provide 'json-hs-extra)
  :config
  (advice-add 'delete-overlay :around #'json-hs-extra-delete-overlays))

(use-package lua-ts-mode
  :defer t
  :when (and (treesit-p)
             (package-installed-p 'lua-ts-mode))
  :mode "\\.lua\\'"
  :custom
  (lua-ts-indent-offset 4)
  :init
  (treesit-install-and-remap
   'lua "https://github.com/MunifTanjim/tree-sitter-lua"
   :modes '(lua-mode)
   :remap 'lua-ts-mode
   :org-src '("lua" . lua-ts)))

(use-package lua-prettify
  :defer t
  :delight lua-prettify-mode
  :unless (in-termux-p)
  :preface
  (defgroup lua-prettify ()
    "Lua prettification and ease of writing enchancements."
    :prefix "lua-prettify-"
    :group 'languages)
  (defcustom lua-prettify-syntax-expansions
    '(("def" "local function")
      ("unless" "if not")
      ("fn"  "function")
      ("let" "local")
      ("<-" "return"))
    "List of abbreviarions and expansions for Lua"
    :type '(repeat (list string string))
    :group 'lua-prettify)
  (defvar lua-prettify--original-syntax-table nil
    "Original Lua syntax table.

Syntax table is modified for abbreviation expansion to work on
characters not considiered as word characters in original Lua table.
This variable holds the original value to be restored once the mode is
disabled.")
  (defun lua-prettify--expand-abbrev-maybe ()
    "Special advise for expanding abbreviations.

Abbrevs that normally don't expand via abbrev-mode are handled manually."
    (when (looking-back "<-" 1)
      (delete-char -2)
      (abbrev-insert (abbrev-symbol "<-"))))
  (defun lua-prettify--cleanup ()
    "Disable Lua prettification."
    (setq prettify-symbols-alist nil)
    (prettify-symbols-mode -1)
    (abbrev-mode -1)
    (remove-function
     (local 'abbrev-expand-function)
     #'lua-prettify--expand-abbrev-maybe)
    (when lua-prettify--original-syntax-table
      (set-syntax-table lua-prettify--original-syntax-table)
      (setq lua-prettify--original-syntax-table nil)))
  (defun lua-prettify--setup ()
    "Setup Lua prettification."
    (setq prettify-symbols-alist
          (mapcar (lambda (abbrev-exp)
                    (let ((abbrev (car abbrev-exp))
                          (exp (cadr abbrev-exp)))
                      `(,exp . ,(thread-last
                                  abbrev
                                  (mapcan
                                   (lambda (ch)
                                     (list '(Br . Bl) ch)))
                                  cdr
                                  vconcat))))
                  lua-prettify-syntax-expansions))
    (prettify-symbols-mode 1)
    (let ((at (eval (intern (format "%s-abbrev-table" major-mode)))))
      (dolist (abbrev-exp lua-prettify-syntax-expansions)
        (apply #'define-abbrev at abbrev-exp)))
    (setq lua-prettify--original-syntax-table (syntax-table))
    (modify-syntax-entry ?- "w 12")
    (abbrev-mode 1)
    (add-function
     :before (local 'abbrev-expand-function)
     #'lua-prettify--expand-abbrev-maybe))
  (define-minor-mode lua-prettify-mode
    "Lua prettification and ease of writing enchancements."
    :lighter " Lua Pretty"
    :init-value nil
    (if (and lua-prettify-mode
             (not current-prefix-arg))
        (lua-prettify--setup)
      (lua-prettify--cleanup)))
  (provide 'lua-prettify))

(use-package elixir-ts-mode
  :defer t
  :when (treesit-p)
  :init
  (treesit-install-and-remap
   'elixir "https://github.com/elixir-lang/tree-sitter-elixir"
   :modes '(elixir-mode)
   :remap 'elixir-ts-mode
   :org-src '("elixir" . elixir-ts)))

(use-package heex-ts-mode
  :defer t
  :when (treesit-p)
  :init
  (treesit-install-and-remap
   'heex "https://github.com/phoenixframework/tree-sitter-heex"))


;;;; LSP

(use-package lsp-mode
  :ensure t
  :hook ((lsp-mode . lsp-diagnostics-mode))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-auto-configure nil)
  (lsp-diagnostics-provider :flymake)
  (lsp-completion-provider :none)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)
  (lsp-keep-workspace-alive nil)
  (lsp-idle-delay 0.5)
  (lsp-enable-xref t)
  (lsp-signature-doc-lines 1))

(use-package lsp-completion
  :no-require
  :hook ((lsp-mode . lsp-completion-mode-maybe))
  :commands (lsp-completion-mode)
  :preface
  (defun lsp-completion-mode-maybe ()
    (unless (bound-and-true-p cider-mode)
      (lsp-completion-mode 1))))

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
  :after lsp-mode)

(use-package lsp-java
  :hook (java-mode . lsp))

(use-package lsp-metals
  :ensure t
  :after lsp-mode
  :custom
  (lsp-metals-server-args
   '("-J-Dmetals.allow-multiline-string-formatting=off"
     "-J-Dmetals.icons=unicode"))
  (lsp-metals-enable-semantic-highlighting nil))

(use-package lsp-metals
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

(use-package region-bindings
  :vc ( :url "https://gitlab.com/andreyorst/region-bindings.el.git"
        :branch "main"
        :rev :newest)
  :commands (region-bindings-mode)
  :preface
  (defun region-bindings-off ()
    (region-bindings-mode -1))
  :hook ((after-init . global-region-bindings-mode)
         ((elfeed-search-mode magit-mode mu4e-headers-mode)
          . region-bindings-off)))

(use-package replace
  :bind ( :map region-bindings-mode-map
          ("k" . keep-lines)
          ("f" . flush-lines)))

(use-package puni
  :ensure t
  :hook (((common-lisp-modes-mode nxml-mode json-ts-mode) . puni-mode)
         (puni-mode . electric-pair-local-mode))
  :bind ( :map region-bindings-mode-map
          ("(" . puni-wrap-round)
          ("[" . puni-wrap-square)
          ("{" . puni-wrap-curly)
          ("<" . puni-wrap-angle)
          ;; paredit-like keys
          :map puni-mode-map
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
          ("M-S" . puni-split)))

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

(use-package phi-search
  :ensure t
  :defer t)

(use-package avy
  :ensure t
  :bind
  ( :map avy-map
    ("M-w l" . avy-kill-ring-save-whole-line)
    ("M-k l" . avy-kill-whole-line)
    ("M-w r" . avy-kill-ring-save-region)
    ("M-k r" . avy-kill-region)
    ("c" . avy-goto-char-timer)
    ("l" . avy-goto-line)
    ("n" . avy-next)
    ("p" . avy-prev))
  :preface
  (defalias 'avy-map-prefix (make-sparse-keymap))
  (defvar avy-map (symbol-function 'avy-map-prefix)
    "Keymap for characters following \\`M-a'.")
  (define-key global-map (kbd "M-a") 'avy-map-prefix))

(use-package isayt
  :vc ( :url "https://gitlab.com/andreyorst/isayt.el.git"
        :branch "main"
        :rev :newest)
  :delight isayt-mode
  :hook (common-lisp-modes-mode . isayt-mode))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

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
  :bind ( :map mode-specific-map
          ("u" . vundo))
  :custom
  (vundo-roll-back-on-quit nil)
  (vundo--window-max-height 10))

(use-package yasnippet
  :ensure t
  :defer t
  :delight yas-minor-mode)


;;; Tools

(use-package ediff
  :defer t
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
     "deps.edn" "shadow-cljs.edn" "deps.fnl"))
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
  (defvar project-compilation-modes nil
    "List of functions to check for specific compilation mode.

The function must return a symbol of an applicable compilation
mode.")
  (define-advice compilation-start
      (:filter-args (args) use-project-compilation-mode)
    (let ((cmd (car args))
          (mode (cadr args))
          (rest (cddr args)))
      (catch 'args
        (when (null mode)
          (dolist (comp-mode-p project-compilation-modes)
            (when-let* ((mode (funcall comp-mode-p)))
              (throw 'args (append (list cmd mode) rest)))))
        args)))
  (define-advice project-root (:filter-return (project) abbreviate-project-root)
    (abbreviate-file-name project))
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
               '(project-switch-to-buffer "Switch buffer"))
  (add-to-list 'project-switch-commands
               '(project-compile "Compile"))
  (add-to-list 'project-switch-commands
               '(project-save-some-buffers "Save") t))

(use-package magit
  :ensure t
  :hook ((git-commit-mode . flyspell-mode)
         (git-commit-mode . magit-git-commit-insert-branch))
  :bind ( :map project-prefix-map
          ("m" . magit-project-status))
  :functions (magit-get-current-branch)
  :custom
  (magit-ediff-dwim-show-on-hunks t)
  (magit-diff-refine-ignore-whitespace t)
  (magit-diff-refine-hunk 'all)
  :preface
  (defun magit-extract-branch-tag (branch-name)
    "Extract branch tag from BRANCH-NAME."
    (let ((ticket-pattern "\\([[:alpha:]]+-[[:digit:]]+\\)"))
      (when (string-match-p ticket-pattern branch-name)
        (upcase (replace-regexp-in-string ticket-pattern "\\1: " branch-name)))))
  (defun magit-git-commit-insert-branch ()
    "Insert the branch tag in the commit buffer if feasible."
    (when-let* ((tag (magit-extract-branch-tag (magit-get-current-branch))))
      (unless
          ;; avoid repeated insertion when amending
          (save-excursion (search-forward (string-trim tag) nil 'no-error))
        (insert tag)))))

(use-package magit
  :after project
  :config
  (add-to-list 'project-switch-commands
               '(magit-project-status "Magit") t))

(use-package magit-todos
  :ensure t
  :when (version<= emacs-version "30.0.91")
  :functions
  (magit-todos-mode)
  :after magit
  :config (magit-todos-mode 1))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode))

(use-package server
  :commands (server-running-p)
  :init
  (unless (server-running-p)
    (server-start)))

(use-package separedit
  :ensure t
  :hook (separedit-buffer-creation . separedit-header-line-setup)
  :bind ( ("C-c '" . separedit)
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
  (dolist (dir (list (locate-user-emacs-file ".cache/")
                     (locate-user-emacs-file "workspace/.cache/")))
    (add-to-list 'recentf-exclude (concat (regexp-quote dir) ".*"))))

(use-package compile
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-scroll-output 'first-error)
  :commands (define-compilation-mode)
  :preface
  (cl-defun compile-add-error-syntax
      (mode name regexp &key file line col (level 'error) hyperlink highlight)
    "Register new compilation error syntax.

Add NAME symbol to `compilation-error-regexp-alist', and then add
REGEXP FILE LINE and optional COL LEVEL info to
`compilation-error-regexp-alist-alist'."
    (or file (error "Missing value for :file keyword"))
    (or line (error "Missing value for :line keyword"))
    (let ((faces '(compilation-info-face
                   compilation-warning-face
                   compilation-error-face))
          (level (cond ((eq level 'info) 0)
                       ((eq level 'warn) 1)
                       ((eq level 'error) 2)
                       (t (error "Mnsupported level type: %S" level))))
          (mode (symbol-name (or mode 'compilation))))
      (add-to-list (intern (concat mode "-error-regexp-alist")) name)
      (add-to-list (intern (concat mode "-error-regexp-alist-alist"))
                   (list name regexp file line col level hyperlink
                         (list highlight (nth level faces))))))
  (defmacro define-project-compilation-mode (base-name &rest body)
    "Define a project-based compilation mode.
BASE-NAME is a symbol that will be used to prefix all mode variables.
BODY is the usual mode setup and teardown code."
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
           (setq-local ,current-project-files (project-files ,current-project-root))
           ,@body)
         (provide ',compilation-mode-name)))))

(use-package clojure-compilation-mode
  :no-require
  :after compile
  :preface
  (declare-function clojure-project-root-path "ext:clojure-mode")
  (defun clojure-compilation-p ()
    (and (require 'clojure-mode nil 'noerror)
         (clojure-project-root-path)
         'clojure-compilation-mode))
  (add-to-list 'project-compilation-modes 'clojure-compilation-p)
  (defun clojure-compilation--split-classpath (classpath)
    "Split the CLASSPATH string."
    (split-string classpath ":" t "[[:space:]\n]+"))
  (defmemo clojure-compilation--get-project-dependencies-memo
      (command _deps-file _mod-time)
    "Call COMMAND to obtain the classpath string.
DEPS-FILE and MOD-TIME are used for memoization."
    (thread-last
      command
      shell-command-to-string
      clojure-compilation--split-classpath
      (seq-filter (lambda (s) (string-suffix-p ".jar" s)))))
  (defun clojure-compilation--get-lein-project-dependencies (root)
    "Obtain classpath from lein for ROOT."
    (let* ((project-file (expand-file-name "project.clj" root))
           (mod-time (file-attribute-modification-time (file-attributes project-file))))
      (clojure-compilation--get-project-dependencies-memo
       "lein classpath" project-file mod-time)))
  (defun clojure-compilation--get-deps-project-dependencies (root)
    "Obtain classpath from deps for ROOT."
    (let* ((project-file (expand-file-name "deps.edn" root))
           (mod-time (file-attribute-modification-time (file-attributes project-file))))
      (clojure-compilation--get-project-dependencies-memo
       "clojure -Spath" project-file mod-time)))
  (defun clojure-compilation-get-project-dependencies (project)
    "Get dependencies of the given PROJECT.
Returns a list of all jar archives."
    (when (bound-and-true-p tramp-gvfs-enabled)
      (let ((root (project-root project)))
        (cond ((file-exists-p (expand-file-name "deps.edn" root))
               (clojure-compilation--get-deps-project-dependencies root))
              ((file-exists-p (expand-file-name "project.clj" root))
               (clojure-compilation--get-lein-project-dependencies root))))))
  (defvar-local clojure-compilation-project-deps nil
    "List of project's dependencies")
  (defvar-local clojure-compilation-project-deps-mod-time nil
    "Accumulated modification time of all project's libraries")
  (define-project-compilation-mode clojure-compilation
    (require 'tramp-gvfs)
    (setq-local clojure-compilation-project-deps
                (clojure-compilation-get-project-dependencies
                 clojure-compilation-current-project))
    (setq-local clojure-compilation-project-deps-mod-time
                (seq-reduce #'+ (mapcar (lambda (f)
                                          (time-to-seconds
                                           (file-attribute-modification-time
                                            (file-attributes f))))
                                        clojure-compilation-project-deps)
                            0)))
  (defun clojure-compilation--find-file-in-project (file)
    "Check if FILE is part of the currently compiled project."
    (if (file-name-absolute-p file)
        file
      (seq-find
       (lambda (s) (string-suffix-p file s))
       clojure-compilation-current-project-files)))
  (defun clojure-compilation--file-exists-jar-p (jar file)
    "Check if FILE is present in the JAR archive."
    (with-temp-buffer
      (when (zerop (call-process "jar" nil (current-buffer) nil "-tf" jar))
        (goto-char (point-min))
        (save-match-data
          (re-search-forward (format "^%s$" (regexp-quote file)) nil t)))))
  (defmemo clojure-compilation--find-dep-memo
      (file _project _deps-mod-time)
    "Find FILE in current project dependency list.
PROJECT and DEPS-MOD-TIME are used for memoizing the call."
    (when (not (string-empty-p file))
      (seq-find (lambda (d)
                  (clojure-compilation--file-exists-jar-p d file))
                clojure-compilation-project-deps)))
  (defun clojure-compilation--find-dep (file)
    "Find FILE in current project dependency list."
    (clojure-compilation--find-dep-memo
     file
     clojure-compilation-current-project
     clojure-compilation-project-deps-mod-time))
  (defun clojure-compilation-filename ()
    "Function that gets filename from the error message.
If the filename comes from a dependency, try to guess the
dependency artifact based on the project's dependencies."
    (when-let* ((filename (substring-no-properties (match-string 1))))
      (or (clojure-compilation--find-file-in-project filename)
          (when-let* ((dep (clojure-compilation--find-dep filename)))
            (concat (expand-file-name dep) "/" filename)))))
  :config
  (compile-add-error-syntax
   'clojure-compilation 'some-warning
   "^\\([^:[:space:]]+\\):\\([0-9]+\\) "
   :file #'clojure-compilation-filename
   :line 2 :level 'warn :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clj-kondo-warning
   "^\\(/[^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): warning"
   :file 1 :line 2 :col 3 :level 'warn :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clj-kondo-error
   "^\\(/[^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): error"
   :file 1 :line 2 :col 3 :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'kaocha-tap
   "^not ok.*(\\([^:]*\\):\\([0-9]*\\))"
   :file #'clojure-compilation-filename
   :line 2 :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clojure-fail
   "^.*\\(?:FAIL\\|ERROR\\) in.*(\\([^:]*\\):\\([0-9]*\\))"
   :file #'clojure-compilation-filename
   :line 2 :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clojure-reflection-warning
   "^Reflection warning,[[:space:]]*\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)"
   :file #'clojure-compilation-filename
   :line 2 :col 3
   :level 'warn :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clojure-performance-warning
   "^Performance warning,[[:space:]]*\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)"
   :file #'clojure-compilation-filename
   :line 2 :col 3
   :level 'warn :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'clojure-syntax-error
   "^Syntax error .* at (\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\))"
   :file #'clojure-compilation-filename
   :line 2 :col 3)
  (compile-add-error-syntax
   'clojure-compilation 'kaocha-unit-error
   "^ERROR in unit (\\([^:]+\\):\\([0-9]+\\))"
   :file #'clojure-compilation-filename
   :line 2 :hyperlink 1 :highlight 1)
  (compile-add-error-syntax
   'clojure-compilation 'eastwood-warning
   "^\\([^:[:space:]]+\\):\\([0-9]+\\):\\([0-9]+\\):"
   :file #'clojure-compilation-filename
   :line 2 :col 3 :level 'warn :hyperlink 1 :highlight 1))

(use-package password-store
  :no-require
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
  :vc ( :url "https://github.com/andreyorst/jdecomp"
        :branch "master"
        :rev :newest)
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

(use-package gnome-proxy
  :vc ( :url "https://gitlab.com/andreyorst/gnome-proxy.el.git"
        :branch "main"
        :rev :newest)
  :when (and (executable-find "gsettings")
             (executable-find "dconf"))
  :delight gnome-proxy-watch-mode
  :hook (after-init . gnome-proxy-watch-mode))

(use-package eat
  :ensure t
  :hook (eshell-load . eat-eshell-mode))

(use-package orderless
  :ensure t
  :defer t
  :custom
  (completion-category-overrides
   '((buffer (styles basic orderless))
     (file (styles basic orderless))
     (project-file (styles basic orderless)))))

;;; Messaging

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
  :no-require
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
  (when (load (locate-user-emacs-file "mail-contexts.el") 'noerror)
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
  :commands (mu4e-alert-enable-notifications)
  :custom
  (mu4e-alert-style 'libnotify)
  (mu4e-alert-icon "emacs")
  :config
  (mu4e-alert-enable-notifications))

(provide 'init)
;;; init.el ends here
