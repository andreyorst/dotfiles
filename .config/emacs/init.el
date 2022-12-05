;;; init.el --- Main configuration file -*- lexical-binding: t; no-byte-compile: t -*-

;; Author: Andrey Listopadov
;; Keywords: Emacs configuration
;; Homepage: https://gitlab.com/andreyorst/dotfiles.git

;;; Commentary:
;; Emacs config.

;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package delight :straight t)

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
    (unless (looking-at (char-to-string ?\^L))
      (forward-page))
    (narrow-to-page))
  (defun narrow-prev-page ()
    "Narrow to the previous page."
    (interactive)
    (widen)
    (unless (looking-at (char-to-string ?\^L))
      (backward-page))
    (backward-page)
    (narrow-to-page))
  (defun in-termux-p ()
    "Detect if Emacs is running in Termux."
    (executable-find "termux-info"))
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
  (defun memoize (fn)
    (let ((memo (make-hash-table :test 'equal)))
      (lambda (&rest args)
        (let ((value (gethash args memo)))
          (or value (puthash args (apply fn args) memo))))))
  (provide 'functions))

(use-package common-lisp-modes
  :delight common-lisp-modes-mode
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
  :hook (after-init . region-bindings-mode-enable)
  :bind ( :map region-bindings-mode-map
          ("q" . region-bindings-disable)
          ("r" . replace-string)
          ("R" . replace-regexp)
          ("\"" . region-bindings-doublequote))
  :preface
  (defun region-bindings-doublequote ()
    "Doublequote active region, escaping strings if needed."
    (interactive)
    (let ((s (buffer-substring-no-properties (point) (mark))))
      (delete-region (point) (mark))
      (insert (format "%S" s))))
  (define-minor-mode region-bindings-mode
    "Minor mode for mapping commands while region is active.

\\<region-bindings-mode-map>"
    :lighter " rbm"
    :group 'convenience
    :keymap (make-sparse-keymap))
  (defun region-bindings-disable ()
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
  (provide 'region-bindings))

(use-package defaults
  :defer t
  :hook (focus-out . garbage-collect)
  :preface
  (setq-default
   indent-tabs-mode nil
   load-prefer-newer t
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

(use-package messages
  :preface
  (provide 'messages)
  :bind ( :map messages-buffer-mode-map
          ("C-c C-o" . messages-clear-buffer))
  :config
  (define-advice message (:around (msg fmt &rest args) log-message-date)
    "Attach a timestamp to messages that go to the *Messages* buffer.

If `message-log-max' is nil just use the original MSG function
with FMT and ARGS.  Otherwise call MSG with timestamp attached,
and then set `message-log-max' so the message would be without
the timestamp in the echo area only."
    (when message-log-max
      (apply msg (concat (format-time-string "[%FT%T.%3N] ") fmt) args))
    (let (message-log-max)
      (apply msg fmt args)))
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

(use-package mode-line
  :defer t
  :preface
  (defvar mode-line-interactive-position
    `(line-number-mode
      (:propertize " %l:%c"
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
  (require-final-newline t)
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
         (not (memq major-mode '(vterm-mode term-mode)))
         (truncated-lines-p)))
  (unless (display-graphic-p)
    (xterm-mouse-mode t)))

(use-package savehist
  :hook (after-init . savehist-mode))

(use-package mule-cmds
  :no-require t
  :custom
  (default-input-method 'russian-computer)
  :init
  (prefer-coding-system 'utf-8))

(use-package select
  :no-require t
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
  :config
  (defun overwrite-mode-set-cursor-shape ()
    (when (display-graphic-p)
      (setq cursor-type (if overwrite-mode 'hollow 'box))))
  :preface
  (unless (fboundp 'minibuffer-keyboard-quit)
    (autoload #'minibuffer-keyboard-quit "delsel" nil t))
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
        (funcall-interactively quit))))
  :init
  (column-number-mode 1)
  (line-number-mode 1))

(use-package delsel
  :hook (after-init . delete-selection-mode))

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
  :defer t
  :custom
  (completion-category-overrides
   '((buffer (styles basic orderless))
     (file (styles basic orderless))
     (project-file (styles basic orderless)))))

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
  :commands (dbus-register-signal dbus-call-method)
  :preface
  (defun color-scheme-changed (path var value)
    "DBus handler to detect when the color-scheme has changed."
    (when (and (string-equal path "org.freedesktop.appearance")
               (string-equal var "color-scheme"))
      (if (equal (car value) '1)
          (load-theme local-config-dark-theme t)
        (load-theme local-config-light-theme t))))
  (defun dark-mode-enabled-p ()
    "Check if dark mode is enabled."
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
  :straight t
  :requires (local-config)
  :custom-face
  (font-lock-doc-face ((t (:foreground unspecified :inherit font-lock-comment-face))))
  (line-number ((t (:foreground unspecified :background unspecified :inherit shadow))))
  (line-number-current-line ((t (:foreground unspecified :weight bold :background unspecified :inherit hl-line))))
  :custom
  (modus-themes-org-blocks nil)
  (modus-themes-syntax '(faint alt-syntax))
  (modus-themes-region '(bg-only no-extend))
  (modus-themes-completions '((matches . (intense bold))
                              (selection . (intense))))
  (modus-themes-operandi-color-overrides '((bg-main . "#fbfcfd")))
  (modus-themes-vivendi-color-overrides `((bg-main . ,(if (in-termux-p) "#000000" "#181818"))))
  (modus-themes-mode-line '(borderless))
  (modus-themes-fringes nil)
  :init
  (cond ((in-termux-p)
         (load-theme local-config-dark-theme t))
        ((and (fboundp 'dark-mode-enabled-p)
              (dark-mode-enabled-p))
         (load-theme local-config-dark-theme t))
        (t (load-theme local-config-light-theme t))))

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

(use-package vertico
  :straight t
  :load-path "straight/repos/vertico/extensions/"
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
  :straight t
  :after vertico
  :config
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
  :load-path "straight/repos/corfu/extensions/"
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
  (global-corfu-mode 1))

(use-package corfu-popupinfo
  :bind ( :map corfu-popupinfo-map
          ("M-p" . corfu-popupinfo-scroll-down)
          ("M-n" . corfu-popupinfo-scroll-up))
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom-face
  (corfu-popupinfo ((t :height 1.0))))

(use-package popon
  :straight ( :type git
              :repo "https://codeberg.org/akib/emacs-popon.git"
              :branch "master")
  :defer t
  :unless (display-graphic-p))

(use-package corfu-terminal
  :straight ( :type git
              :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"
              :branch "master")
  :unless (display-graphic-p)
  :after corfu
  :config
  (corfu-terminal-mode 1))

(use-package cape
  :straight t
  :after corfu
  :config
  (setq completion-at-point-functions
        '(cape-file cape-dabbrev)))

(use-package org
  :straight (:type built-in)
  :hook ((org-mode . org-change-syntax-table)
         ((org-capture-mode org-src-mode) . discard-history)
         (org-babel-after-execute . org-redisplay-inline-images))
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
  :preface
  (define-advice org-return (:around (f &rest args) preserve-indentation)
    (let ((org-src-preserve-indentation t))
      (apply f args)))
  (define-advice org-cycle (:around (f &rest args) preserve-indentation)
    (let ((org-src-preserve-indentation t))
      (apply f args)))
  (defun discard-history ()
    "Discard undo history of org src and capture blocks."
    (setq buffer-undo-list nil)
    (set-buffer-modified-p nil))
  (defun org-change-syntax-table ()
    (modify-syntax-entry ?< "w")
    (modify-syntax-entry ?> "w"))
  :config
  (defun org-babel-edit-prep:emacs-lisp (_)
    "Setup Emacs Lisp buffer for Org Babel."
    (setq lexical-binding t))
  (unless (version<= org-version "9.1.9")
    (add-to-list 'org-modules 'org-tempo)))

(use-package ob-shell
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t))))

(use-package blog
  :commands (blog-publish-file
             blog-generate-file-name
             blog-read-list-items)
  :preface
  (defvar blog-capture-template
    "#+hugo_base_dir: ../
#+hugo_section: posts
#+hugo_auto_set_lastmod: t
#+options: tex:dvisvgm
#+macro: kbd @@html:<kbd>$1</kbd>@@

#+title: %(format \"%s\" blog--current-post-name)
#+date: %(format-time-string \"%Y-%m-%d %h %H:%M\")
#+hugo_tags: %(blog-read-list-items \"Select tags: \" 'blog-tags)
#+hugo_categories: %(blog-read-list-items \"Select categories: \" 'blog-categories)
#+hugo_custom_front_matter: :license %(format \"%S\" blog-license)

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
        (format "%s-%s.org"
                (format-time-string "%Y-%m-%d")
                (blog-title-to-fname title))))))
  (defun blog-publish-file ()
    "Update '#+date:' tag, and rename the currently visited file.
File name is updated to include the same date and current title."
    (interactive)
    (save-match-data
      (let ((today (format-time-string "%Y-%m-%d"))
            (now (format-time-string "%h %H:%M")))
        (save-excursion
          (goto-char (point-min))
          (re-search-forward "^#\\+date:.*$")
          (replace-match (format "#+date: %s %s" today now)))
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
          (save-buffer)))))
  (provide 'blog))

(use-package org-capture
  :defer t
  :custom
  (org-directory blog-directory)
  (org-capture-templates `(("p" "Post" plain
                            (function blog-generate-file-name)
                            ,blog-capture-template
                            :jump-to-captured t
                            :immediate-finish t))))

(use-package org-tree-slide
  :straight t
  :defer t
  :custom
  (org-tree-slide-slide-in-effect nil)
  (org-tree-slide-never-touch-face t))

(use-package org-modern
  :straight t
  :defer t
  :hook (org-tree-slide-mode . org-modern-mode)
  :custom-face
  (org-modern-block-name ((t (:height 1.0))))
  (org-modern-label ((t (:height 1.0))))
  :custom
  (org-modern-hide-stars t)
  (org-modern-block-fringe nil))

(use-package ox-hugo
  :straight ( :host github
              :repo "kaushalmodi/ox-hugo"
              :branch "main")
  :after ox)

(use-package ox-latex
  :after ox)

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
         (emacs-lisp-mode . common-lisp-modes-mode)
         (emacs-lisp-mode . setup-imenu-use-package-support))
  :config
  (defun setup-imenu-use-package-support ()
    "Add `use-package' support to `imenu'."
    (add-to-list
     'imenu-generic-expression
     '("use-package"
       "\\s(use-package[[:space:]]+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)"
       1)
     t)))

(use-package fennel-mode
  :straight ( :type git
              :host nil
              :branch "main"
              :repo "https://git.sr.ht/~technomancy/fennel-mode")
  :hook ((fennel-mode fennel-repl-mode) . common-lisp-modes-mode)
  :bind ( :map fennel-mode-map
          ("M-." . xref-find-definitions)
          ("M-," . xref-go-back)
          :map fennel-repl-mode-map
          ("C-c C-o" . fennel-repl-delete-all-output))
  :custom
  (fennel-eldoc-fontify-markdown t)
  :preface
  (defun fennel-repl-delete-all-output ()
    (interactive)
    (save-excursion
      (goto-char (process-mark (get-buffer-process (current-buffer))))
      (forward-line 0)
      (let ((inhibit-read-only t))
        (delete-region (point) (point-min)))))
  :config
  (dolist (sym '(global local var))
    (put sym 'fennel-indent-function 1)))

(use-package fennel-mode
  :no-require t
  :after (smartparens-config fennel-mode)
  :config
  (defvar fennel-modes
    '(fennel-mode fennel-repl-mode)
    "List of Fennel-related modes")
  (sp-with-modes fennel-modes
    (sp-local-pair "`" "`"
                   :when '(sp-in-string-p sp-in-comment-p)
                   :unless '(sp-lisp-invalid-hyperlink-p)))
  (defvar fennel-prefix "\\(?:[@`'#~,_?^]+\\)"
    "Prefix used in `sp-sexp-prefix' for Fennel modes.")
  (dolist (mode fennel-modes)
    (add-to-list 'sp-sexp-prefix `(,mode regexp ,fennel-prefix))))

(use-package ob-fennel
  :straight ( :host gitlab
              :repo "andreyorst/ob-fennel"
              :branch "main")
  :after (org fennel-mode)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((fennel . t))))

(use-package clojure-mode
  :straight t
  :hook ((clojure-mode
          clojurec-mode
          clojurescript-mode)
         . clojure-mode-setup)
  :commands (clojure-project-dir)
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
  :straight t
  :delight " CIDER"
  :functions flycheck-mode
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
  (put 'cider-clojure-cli-aliases 'safe-local-variable #'listp)
  (defun cider-disable-linting ()
    "Disable linting integrations for current buffer."
    (when (bound-and-true-p flycheck-mode)
      (flycheck-mode -1))
    (when (bound-and-true-p flymake-mode)
      (flymake-mode -1)))
  (defun cider-repl-prompt-newline (namespace)
    "Return a prompt string that mentions NAMESPACE with a newline."
    (format "%s\n> " namespace)))

(use-package cider
  :no-require t
  :after (org cider)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t))))

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
  :commands (lisp-eval-last-sexp)
  :custom
  (inferior-lisp-program (cond ((executable-find "sbcl") "sbcl")
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
  :straight t
  :hook (sly-mrepl-mode . common-lisp-modes-mode)
  :commands (sly-symbol-completion-mode)
  :config
  (sly-symbol-completion-mode -1))

(use-package scheme
  :hook (scheme-mode . common-lisp-modes-mode))

(use-package geiser
  :straight t
  :hook (scheme-mode . geiser-mode)
  :custom
  (geiser-active-implementations '(guile))
  (geiser-default-implementation 'guile))

(use-package geiser-guile
  :straight t
  :after geiser)

(use-package racket-mode
  :straight t
  :hook ((racket-mode racket-repl-mode) . common-lisp-modes-mode))

(use-package yaml-mode
  :straight t
  :defer t
  :custom
  (yaml-indent-offset 4))

(use-package lua-mode
  :straight t
  :defer t
  :custom
  (lua-indent-level 2))

(use-package ob-lua
  :after (org lua-mode)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lua . t))))

(use-package css-mode
  :defer t
  :custom
  (css-indent-offset 2))

(use-package json-mode
  :defer t
  :custom
  (js-indent-level 2))

(use-package csv-mode
  :straight t
  :defer t
  :custom
  (csv-align-max-width 80))

(use-package erlang
  :load-path "/usr/share/emacs/site-lisp/erlang/"
  :mode ("\\.erl\\'" . erlang-mode))

(use-package elixir-mode
  :straight t
  :defer t)

(use-package zig-mode
  :straight t
  :defer t)

(use-package scala-mode
  :straight t
  :defer t)

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

(use-package flyspell
  :straight t
  :when (or (executable-find "ispell")
            (executable-find "aspell")
            (executable-find "hunspell"))
  :hook ((org-mode git-commit-mode markdown-mode) . flyspell-mode))

(use-package puni
  :straight t
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
          ("M-[" . puni-wrap-square)
          ("M-{" . puni-wrap-curly)
          ("M-?" . puni-convolute)
          ("M-S" . puni-split)
          :map region-bindings-mode-map
          ("(" . puni-wrap-round)
          ("[" . puni-wrap-square)
          ("{" . puni-wrap-curly)
          ("<" . puni-wrap-angle))
  :config
  (define-advice puni-kill-line (:before (&rest _))
    "Go back to indentation before killing the line if it makes sense to."
    (when (looking-back "^[[:space:]]*")
      (back-to-indentation))))

(use-package vundo
  :straight ( :host github
              :repo "casouri/vundo"
              :branch "master")
  :bind (("C-c u" . vundo))
  :custom
  (vundo-roll-back-on-quit nil)
  (vundo--window-max-height 10))

(use-package ediff
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (advice-add 'ediff-window-display-p :override #'ignore))

(use-package project
  :straight t
  :bind ( :map project-prefix-map
          ("s" . project-save-some-buffers))
  :commands (project-root)
  :custom
  (project-compilation-buffer-name-function 'project-prefixed-buffer-name)
  :preface
  (defcustom project-root-markers
    '("Cargo.toml" "compile_commands.json" "compile_flags.txt"
      "project.clj" ".git" "deps.edn" "shadow-cljs.edn")
    "Files or directories that indicate the root of a project."
    :type '(repeat string)
    :group 'project)
  (defun project-root-p (path)
    "Check if the current PATH has any of the project root markers."
    (catch 'found
      (dolist (marker project-root-markers)
        (when (file-exists-p (concat path marker))
          (throw 'found marker)))))
  (defun project-find-root (path)
    "Search up the PATH for `project-root-markers'."
    (when-let ((root (locate-dominating-file path #'project-root-p)))
      (cons 'transient (expand-file-name root))))
  (defun project-save-some-buffers (&optional arg)
    "Save some modified file-visiting buffers in the current project.

Optional argument ARG (interactively, prefix argument) non-nil
means save all with no questions."
    (interactive "P")
    (let* ((project-buffers (project-buffers (project-current)))
           (pred (lambda () (memq (current-buffer) project-buffers))))
      (funcall-interactively #'save-some-buffers arg pred)))
  (define-advice project-compile (:around (fn) save-project-buffers)
    "Only ask to save project-related buffers."
    (defvar compilation-save-buffers-predicate)
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
  :config
  (add-to-list 'project-switch-commands
               '(project-dired "Dired"))
  (add-to-list 'project-switch-commands
               '(project-switch-to-buffer "Switch buffer"))
  (add-to-list 'project-find-functions #'project-find-root))

(use-package toggle-test
  :straight t
  :after (project)
  :bind ( :map project-prefix-map
          ("T" . tgt-toggle))
  :custom
  (tgt-open-in-new-window nil)
  :preface
  (defun tgt-local-setup (&optional spec)
    (lambda ()
      (when-let* ((root (project-current))
                  (project-root (project-root root)))
        (setq-local
         tgt-projects
         `(((:root-dir ,project-root)
            (:src-dirs ,(or (plist-get spec :src-dirs) "src"))
            (:test-dirs ,(or (plist-get spec :test-dirs) "test"))
            (:test-suffixes ,(plist-get spec :suffixes))
            (:test-prefixes ,(plist-get spec :prefixes))))))))
  :init
  (dolist (hook '(clojure-mode-hook
                  clojurec-mode-hook
                  clojurescript-mode-hook))
    (add-hook hook (tgt-local-setup '(:suffixes "_test"))))
  (add-hook 'fennel-mode-hook
            (tgt-local-setup '(:test-dirs "tests" :suffixes "-test"))))

(use-package vterm
  :straight t
  :when (bound-and-true-p module-file-suffix)
  :bind ( :map vterm-mode-map
          ("<insert>" . ignore)
          ("<f2>" . ignore)
          :map project-prefix-map
          ("t" . vterm-project-dir))
  :custom
  (vterm-always-compile-module t)
  (vterm-environment '("VTERM=1"))
  :preface
  (unless (fboundp 'project-prefixed-buffer-name)
    (autoload #'project-prefixed-buffer-name "project"))
  (defun vterm-project-dir (&optional _)
    "Launch vterm in current project.

Opens an existing vterm buffer for a project if present, unless
the prefix argument is supplied."
    (interactive "P")
    (let* ((default-directory (project-root (project-current t)))
           (name (project-prefixed-buffer-name "vterm")))
      (if (and (not current-prefix-arg) (get-buffer name))
          (switch-to-buffer name)
        (funcall-interactively #'vterm name))))
  :init
  (require 'project)
  (add-to-list 'project-switch-commands
               '(vterm-project-dir "vterm") t))

(use-package magit
  :straight t
  :hook (git-commit-mode . flyspell-mode)
  :bind ( :map project-prefix-map
          ("m" . magit-project-status))
  :custom
  (magit-ediff-dwim-show-on-hunks t)
  (magit-diff-refine-ignore-whitespace t)
  (magit-diff-refine-hunk 'all)
  :init
  (require 'project)
  (add-to-list 'project-switch-commands
               '(magit-project-status "Magit") t))

(use-package magit-todos
  :straight t
  :after magit
  :custom
  (magit-todos-nice (and (executable-find "nice") t))
  :config
  (magit-todos-mode 1))

(use-package server
  :commands (server-running-p)
  :init
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

(use-package dumb-jump
  :straight t
  :defer t
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'completing-read)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package gcmh
  :straight t
  :hook (after-init . gcmh-mode)
  :delight gcmh-mode)

(use-package paren
  :hook (prog-mode . show-paren-mode))

(use-package vc-hooks
  :defer t
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
  :defer t
  :custom
  (eldoc-echo-area-use-multiline-p nil))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

(use-package hl-todo
  :straight t
  :hook (prog-mode . hl-todo-mode))

(use-package compile
  :hook
  (compilation-filter . ansi-color-compilation-filter)
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-error-regexp-alist nil)
  :preface
  (defun compile-add-error-syntax (name regexp file line &optional col level)
    "Register new compilation error syntax.

Add NAME symbol to `compilation-error-regexp-alist', and then add
REGEXP FILE LINE and optional COL LEVEL info to
`compilation-error-regexp-alist-alist'."
    (add-to-list 'compilation-error-regexp-alist name)
    (add-to-list 'compilation-error-regexp-alist-alist
                 (list name regexp file line col level)))
  (defun compile-clojure-filename-fn (regexp)
    "Create a function that gets filename from the error message.

REGEX is a regular expression to extract filename from first
group via backward search."
    (lambda ()
      "Get a filname from the error message and compute relative directory.

If the filename ends with _test.clj, the relative directory
returned is test, otherwise it's src."
      (let* ((filename (save-match-data
                         (re-search-backward regexp)
                         (substring-no-properties (match-string 1))))
             (dir (if (string-suffix-p "_test.clj" filename)
                      "test"
                    "src")))
        (message "%S" (cons filename dir))
        (cons filename dir))))
  :config
  (when (fboundp #'ansi-color-compilation-filter)
    (add-hook 'compilation-filter #'ansi-color-compilation-filter))
  (compile-add-error-syntax
   'kaocha-tap
   "^not ok.*(\\([^:]*\\):\\([0-9]*\\))$"
   (compile-clojure-filename-fn "(\\([^(:]*\\):[0-9]*)")
   2)
  (compile-add-error-syntax
   'clojure-fail
   ".*\\(?:FAIL\\|ERROR\\) in.*(\\([^:]*\\):\\([0-9]*\\))$"
   (compile-clojure-filename-fn "(\\([^:]*\\):[0-9]*)")
   2)
  (compile-add-error-syntax
   'clojure-reflection-warning
   "^Reflection warning,[[:space:]]*\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\).*$"
   (compile-clojure-filename-fn
    "^Reflection warning,[[:space:]]*\\([^:]+\\):[0-9]+:[0-9]+.*$")
   2 3)
  (compile-add-error-syntax
   'clojure-syntax-error
   "^Syntax error .* at (\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\))\.$"
   (compile-clojure-filename-fn "(\\([^:]+\\):[0-9]+:[0-9]+)")
   2 3)
  (compile-add-error-syntax
   'kaocha-unit-error
   "^ERROR in unit (\\([^:]+\\):\\([0-9]+\\))$"
   (compile-clojure-filename-fn "(\\([^:]+\\):[0-9]+)")
   2)
  (compile-add-error-syntax 'lua-stacktrace
                            "\\(?:^[[:space:]]+\\([^
:]+\\):\\([[:digit:]]+\\):[[:space:]]+in.+$\\)"
                            1 2)
  (compile-add-error-syntax 'fennel-compile-error
                            "^\\(?:Compile error in \\(.*\.fnl\\):\\([[:digit:]]+\\):?\\([[:digit:]]+\\)?\\)$"
                            1 2 3))

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

(use-package esh-mode
  :hook (eshell-mode . common-lisp-modes-mode)
  :custom
  (eshell-scroll-show-maximum-output nil))

(use-package dired
  :bind ( :map dired-mode-map
          ("<backspace>" . dired-up-directory)
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

(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :straight t
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

(use-package phi-search
  :straight t
  :defer t)

(use-package profiler
  :straight t
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
  :config
  (define-advice hs-toggle-hiding (:before (&rest _) move-point-to-mouse)
    "Move point to the location of the mouse pointer."
    (mouse-set-point last-input-event)))

(use-package yasnippet
  :straight t
  :defer t
  :delight yas-minor-mode)

(use-package lsp-mode
  :straight t
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

(use-package lsp-clojure
  :no-require t
  :hook ((clojure-mode
          clojurec-mode
          clojurescript-mode)
         . lsp))

(use-package lsp-clojure
  :demand t
  :after lsp-mode
  :hook (cider-mode . cider-toggle-lsp-completion-maybe)
  :preface
  (defun cider-toggle-lsp-completion-maybe ()
    (lsp-completion-mode (if cider-mode -1 1))))

(use-package lsp-treemacs
  :straight t
  :defer t
  :custom
  (lsp-treemacs-theme "Iconless"))

(use-package lsp-java
  :straight t
  :demand t
  :after lsp-mode
  :when (and openjdk-11-path
             (file-exists-p openjdk-11-path))
  :custom
  (lsp-java-java-path openjdk-11-path))

(use-package lsp-java
  :no-require t
  :hook (java-mode . lsp))

(use-package jdecomp
  :straight t
  :mode ("\\.class\\'" . jdecomp-mode)
  :preface
  (defvar cfr-path
    (file-truename "~/.local/lib/cfr.jar")
    "Path to the cfr Java decompiler library.")
  (defvar fernflower-path
    (file-truename "~/.local/lib/fernflower.jar")
    "Path to the FernFlower library.")
  :custom
  (jdecomp-decompiler-type (cond ((file-exists-p cfr-path)
                                  'cfr)
                                 ((file-exists-p fernflower-path)
                                  'fernflower)
                                 (t jdecomp-decompiler-type)))
  (jdecomp-decompiler-paths `((cfr . ,cfr-path)
                              (fernflower . ,fernflower-path))))

(use-package languagetool
  :straight t
  :when (and langtool-installation-dir
             (file-exists-p langtool-installation-dir))
  :commands (languagetool-server-mode
             languagetool-server-start)
  :preface
  (defvar langtool-args
    (when-let* ((ngrams (and langtool-ngrams-dir-name
                             langtool-installation-dir
                             (expand-file-name
                              langtool-ngrams-dir-name
                              langtool-installation-dir)))
                (ngrams (and (file-exists-p ngrams) ngrams)))
      (list (concat "--languagemodel " ngrams))))
  :custom
  (languagetool-server-command
   (expand-file-name "languagetool-server.jar"
                     langtool-installation-dir))
  (languagetool-server-arguments langtool-args))

(use-package erc
  :defer t
  :custom
  (erc-hide-list '("JOIN" "PART" "QUIT"))
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
  (when-let ((hook (erc-setup-port-forwarding erc-tunnel-conf)))
    (add-hook 'erc-connect-pre-hook hook)))

(use-package message
  :defer t
  :custom
  (message-kill-buffer-on-exit t))

(use-package smtpmail
  :defer t)

(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :when (executable-find "mu")
  :defines mu4e-personal-addresses
  :commands (mu4e mu4e-ask-maildir make-mu4e-context mu4e-context-current mu4e-get-maildirs)
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
  (when (load (expand-file-name "mail-contexts.el" user-emacs-directory) 'noerror)
    (setq mu4e-contexts (mapcar #'make-context mail-contexts)
          user-mail-address (plist-get (car mail-contexts) :address)
          mu4e-personal-addresses (mapcar (lambda (ctx) (plist-get ctx :address))
                                          mail-contexts))))

(use-package mu4e
  :no-require t
  :after (orderless mu4e)
  :config
  (define-advice mu4e-ask-maildir (:around (fn prompt) use-orderless)
    (let ((completion-styles (append completion-styles '(orderless))))
      (funcall fn prompt))))

(use-package message-view-patch
  :straight t
  :hook (gnus-part-display . message-view-patch-highlight))

(use-package password-store
  :when (executable-find "pass")
  :commands (password-store-copy
             password-store-get
             password-store-insert
             password-store-generate)
  :load-path "/usr/share/doc/pass/emacs/"
  :config
  (define-advice password-store--completing-read
      (:around (fn &optional require-match) use-orderless)
    (let ((completion-styles (append completion-styles '(orderless))))
      (funcall fn require-match))))

(use-package aoc
  :straight ( :host github
              :repo "pkulev/aoc.el"
              :branch "main")
  :defer t)

(provide 'init)
;;; init.el ends here
