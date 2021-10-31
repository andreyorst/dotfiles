;;; init.el --- Main configuration file -*- lexical-binding: t; -*-

;; Author: Andrey Listopadov
;; Keywords: Emacs configuration
;; Homepage: https://gitlab.com/andreyorst/dotfiles/-/tree/master/.config/emacs

;;; Commentary:
;; Emacs config.

;;; Code:

;;; early-init.el support for older Emacs


;;; Early init support for older Emacs

(unless (featurep 'early-init)
  (load (expand-file-name "early-init" user-emacs-directory)))



;;; use-package

(require 'straight)
(straight-use-package 'use-package)
(defvar straight-use-package-by-default)
(setq straight-use-package-by-default t)
(require 'use-package)



;;; Local config and personal functions

(use-package local-config
  :straight nil
  :preface
  (defgroup local-config ()
    "Customization group for local settings."
    :prefix "local-config-"
    :group 'emacs)

  (defcustom aorst-title-show-bufname t
    "Whether to include bufname to titlebar.
Bufname is not necessary on GNOME, but may be useful in other DEs."
    :type 'boolean
    :group 'local-config)

  (defcustom aorst-dark-theme 'modus-vivendi
    "Dark theme to use."
    :tag "Dark theme"
    :type 'symbol
    :group 'local-config)

  (defcustom aorst-light-theme 'modus-operandi
    "Light theme to use."
    :tag "Light theme"
    :type 'symbol
    :group 'local-config)

  (defcustom aorst-line-pixel-height (line-pixel-height)
    "Line height in pixels.
Used in various places to avoid getting wrong line height when
`text-scale-mode' is active."
    :tag "Line pixel height"
    :type 'number
    :group 'local-config)

  (provide 'local-config))


(use-package functions
  :straight nil
  :preface
  (defun aorst/split-pararagraph-into-lines ()
    "Split current paragraph into lines with one sentence each."
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

  (defun aorst/dark-mode-p ()
    "Check if frame is dark or not."
    (if (and (window-system)
             (executable-find "gsettings"))
        (thread-last "gsettings get org.gnome.desktop.interface gtk-theme"
                     shell-command-to-string
                     string-trim-right
                     (string-suffix-p "-dark'"))
      (eq 'dark (frame-parameter nil 'background-mode))))

  (defun aorst/termuxp ()
    "Detect if Emacs is running in Termux."
    (executable-find "termux-info"))

  (defmacro aorst/add-compilation-error-syntax (name regexp file line &optional col level)
    "Register new compilation error syntax.

Add NAME symbol to `compilation-error-regexp-alist', and then add
REGEXP FILE LINE and optional COL LEVEL info to
`compilation-error-regexp-alist-alist'."
    (declare (indent 1))
    `(progn (add-to-list 'compilation-error-regexp-alist ',name)
            (add-to-list 'compilation-error-regexp-alist-alist
                         '(,name ,regexp ,file ,line ,col ,level))))

  (provide 'functions))


(use-package common-lisp-modes-mode
  :straight nil
  :preface
  (define-minor-mode common-lisp-modes-mode
    "Mode for enabling all modes that are common for lisps.

For reference, this is not a common-lisp modes mode, but a common
lisp-modes mode.

\\<common-lisp-modes-mode-map>"
    :lighter " clmm")

  (provide 'common-lisp-modes-mode))



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
                   (not aorst-title-show-bufname))
               "Emacs"
             (if (buffer-modified-p)
                 "%b* — Emacs"
               "%b — Emacs")))
   auto-window-vscroll nil
   mouse-highlight nil
   hscroll-step 1
   hscroll-margin 1
   scroll-margin 1
   scroll-preserve-screen-position nil)

  (when (window-system)
    (setq-default
     x-gtk-use-system-tooltips nil
     cursor-type 'box
     cursor-in-non-selected-windows nil))

  (setq
   ring-bell-function 'ignore
   mode-line-percent-position nil)

  (when (version<= "27.1" emacs-version)
    (setq bidi-inhibit-bpa t))

  (provide 'defaults))


(use-package font
  :straight nil
  :preface
  (defun aorst/font-installed-p (font-name)
    "Check if font with FONT-NAME is available."
    (find-font (font-spec :name font-name)))

  (cond ((aorst/font-installed-p "JetBrainsMono")
         (set-face-attribute 'default nil :font "JetBrainsMono 10"))
        ((aorst/font-installed-p "Source Code Pro")
         (set-face-attribute 'default nil :font "Source Code Pro 10")))

  (when (aorst/font-installed-p "DejaVu Sans")
    (set-face-attribute 'variable-pitch nil :font "DejaVu Sans 10"))

  (provide 'font))


(use-package cus-edit
  :straight nil
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :init
  (load custom-file :noerror))


(use-package novice
  :straight nil
  :init
  (defvar aorst--disabled-commands (expand-file-name "disabled.el" user-emacs-directory)
    "File to store disabled commands, that were enabled permamently.")

  (define-advice enable-command (:around (foo command))
    (let ((user-init-file aorst--disabled-commands))
      (funcall foo command)))

  (load aorst--disabled-commands :noerror))


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
  (mouse-wheel-flip-direction t)
  (mouse-wheel-tilt-scroll t)
  (mouse-wheel-progressive-speed nil)
  :init
  (global-set-key (kbd "<mouse-3>") menu-bar-edit-menu)

  (defun aorst/truncated-lines-p ()
    "Non-nil if any line is longer than `window-width' + `window-hscroll'.
Returns t if any line exceeds right border of the window.  Used
for stopping scroll from going beyond the longest line.  Based on
`so-long-detected-long-line-p'."
    (save-excursion
      (goto-char (point-min))
      (let* ((window-width
              ;; this computes a more accurate width rather than `window-width', and respects
              ;; `text-scale-mode' font width.
              (/ (window-body-width nil t) (window-font-width)))
             (hscroll-offset
              ;; `window-hscroll' returns columns that are not affected by `text-scale-mode'.
              ;; Because of that we have to recompute correct `window-hscroll' by multiplying
              ;; it with a non-scaled value, and divide with scaled width value, and round it
              ;; to upper boundary.  Since there's no way to get unscaled value, we have to
              ;; get a width of a face that is not scaled by `text-scale-mode', such as
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
               (aorst/truncated-lines-p))
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


(use-package startup
  :straight nil
  :no-require t
  :custom
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message ""))


(use-package simple
  :straight nil
  :bind (("M-z" . zap-up-to-char)
         ("M-S-z" . zap-to-char)
         ("C-x k" . kill-this-buffer)
         ("C-h C-f" . describe-face)
         ("<f2>" . ignore))
  :hook ((before-save . delete-trailing-whitespace)
         (overwrite-mode . aorst/overwrite-set-cursor-shape))
  :custom
  (yank-excluded-properties t)
  (blink-matching-delay 0)
  (blink-matching-paren t)
  :config
  (defun aorst/overwrite-set-cursor-shape ()
    (when (display-graphic-p)
      (setq cursor-type (if overwrite-mode 'hollow 'box))))
  :init
  (column-number-mode 1)
  (line-number-mode 1)
  (transient-mark-mode -1)

  (define-advice keyboard-quit
      (:around (quit))
    "Quit in current context.

When there is an active minibuffer and we are not inside it close
it.  When we are inside the minibuffer use the regular
`minibuffer-keyboard-quit' which quits any active region before
exiting.  When there is no minibuffer `keyboard-quit' unless we
are defining or executing a macro."
    (cond ((active-minibuffer-window)
           (if (minibufferp)
               (minibuffer-keyboard-quit)
             (abort-recursive-edit)))
          (t
           (unless (or defining-kbd-macro
                       executing-kbd-macro)
             (funcall quit))))))


(use-package delsel
  :straight nil
  :init
  (delete-selection-mode t))


(use-package minibuffer
  :straight nil
  :bind (:map minibuffer-inactive-mode-map
         ("<mouse-1>" . ignore))
  :custom
  (completion-styles '(partial-completion basic))
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :custom-face
  (completions-first-difference ((t (:inherit unspecified)))))



;;; UI

(use-package window
  :straight nil
  :bind ("C-x C-b" . bury-buffer))


(use-package frame
  :straight nil
  :requires seq
  :config
  (define-advice toggle-frame-fullscreen
      (:before (&optional frame))
    "Hide menu-bar when FRAME goes full screen."
    (set-frame-parameter
     nil 'menu-bar-lines
     (if (memq (frame-parameter frame 'fullscreen) '(fullscreen fullboth)) 1 0)))

  (define-advice switch-to-buffer-other-frame
      (:around (fn buffer-or-name &optional norecord))
    "Clone fame parameters when switching to other frame."
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
  (tooltip-y-offset aorst-line-pixel-height)
  (tooltip-frame-parameters `((name . "tooltip")
                              (internal-border-width . 2)
                              (border-width . 1)
                              (no-special-glyphs . t))))


(use-package tool-bar
  :straight nil
  :when (window-system)
  :init
  (tool-bar-mode -1))


(use-package scroll-bar
  :straight nil
  :when (window-system)
  :init
  (scroll-bar-mode -1))


(use-package modus-themes
  :requires (functions local-config)
  :functions (aorst/dark-mode-p aorst/termuxp)
  :defines (aorst-dark-theme aorst-light-theme)
  :custom-face
  ;; The `modus-themes-mode-line' custom doesn't allow disabling box
  ;; effect, only recolors it, so it is disabled via custom.
  (mode-line ((t (:box unspecified))))
  (mode-line-inactive ((t (:box unspecified))))
  (font-lock-doc-face ((t (:foreground nil :inherit font-lock-comment-face))))
  :custom
  (modus-themes-org-blocks nil)
  (modus-themes-syntax '(faint alt-syntax))
  (modus-themes-region '(bg-only no-extend))
  (modus-themes-operandi-color-overrides '((bg-main . "#fcfbfa") (fg-main . "#101010")))
  (modus-themes-vivendi-color-overrides (if (aorst/termuxp)
                                            '((bg-main . "#000000") (fg-main . "#e5e6e7"))
                                          '((bg-main . "#252423") (fg-main . "#dedddc"))))
  (modus-themes-completions 'opinionated)
  :init
  (cond ((aorst/termuxp)
         (load-theme aorst-dark-theme t))
        ((aorst/dark-mode-p)
         (load-theme aorst-dark-theme t))
        (t (load-theme aorst-light-theme t))))


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


(use-package minions
  :commands minions-mode
  :custom
  (minions-direct '(flycheck-mode flymake-mode))
  :init
  (minions-mode))


(use-package vertico
  :commands vertico-mode
  :init
  (vertico-mode))


(use-package marginalia
  :commands marginalia-mode
  :init
  (marginalia-mode))


(use-package consult
  :commands consult-completion-in-region
  :requires seq
  :bind (("C-x C-r" . consult-recent-file))
  :config
  (setq consult-preview-excluded-hooks (seq-union consult-preview-excluded-hooks '(lsp)))
  :init
  (setq completion-in-region-function #'consult-completion-in-region))


(use-package company
  :bind (:map company-mode-map
         ([remap completion-at-point] . company-complete)
         ("M-/" . company-complete)
         :map company-active-map
         ("TAB" . company-complete-common-or-cycle)
         ("<tab>" . company-complete-common-or-cycle)
         ("<S-Tab>" . company-select-previous)
         ("<backtab>" . company-select-previous)
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-d" . company-show-doc-buffer)
         ("M-." . company-show-location))
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0)
  (company-require-match 'never)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  (company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                       company-preview-frontend
                       company-echo-metadata-frontend))
  (company-backends '(company-capf company-files company-dabbrev-code))
  (company-tooltip-minimum-width 30)
  (company-tooltip-maximum-width 120)
  (company-icon-size aorst-line-pixel-height))


(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode)
  :custom
  (company-quickhelp-max-lines 13)
  (company-quickhelp-use-propertized-text t))


(use-package formfeed
  :straight nil
  :preface
  (defun aorst/formfeed-line ()
    "Display the formfeed ^L char as comment or as continuous line."
    (unless buffer-display-table
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^L
          (vconcat (make-list (or fill-column 70)
                              (make-glyph-code
                               (string-to-char (or comment-start "-"))
                               'shadow)))))

  (dolist (mode-hook '(help-mode-hook
                       org-mode-hook
                       outline-mode-hook
                       prog-mode-hook))
    (add-hook mode-hook #'aorst/formfeed-line))

  (provide 'formfeed))



;;; Languages

(use-package org
  :straight (:type built-in)
  :hook ((org-capture-mode org-src-mode) . aorst/discard-history)
  :bind (:map org-mode-map
         ("M-Q" . aorst/split-pararagraph-into-lines)
         ("C-c l" . org-store-link))
  :custom-face
  (org-block ((t (:extend t))))
  (org-block-begin-line ((t (:slant unspecified
                             :weight normal
                             :background unspecified
                             :inherit org-block
                             :extend t))))
  (org-block-end-line ((t (:slant unspecified
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
  :config
  (defun aorst/discard-history ()
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
  :hook ((c-mode-common . aorst/cc-mode-setup))
  :config
  (defun aorst/cc-mode-setup ()
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
  :bind (:map markdown-mode-map
         ("M-Q" . aorst/split-pararagraph-into-lines))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-command "pandoc")
  (markdown-hr-display-char nil)
  (markdown-list-item-bullets '("-")))


(use-package geiser
  :hook (scheme-mode . geiser-mode)
  :custom
  (geiser-active-implementations '(guile))
  (geiser-default-implementation 'guile))


(use-package eros
  :straight (:host github
             :repo "andreyorst/eros"
             :branch "general-purpose-api")
  :hook ((emacs-lisp-mode . eros-mode)
         (fennel-mode . aorst/enable-fennel-eros))
  :defines inferior-lisp-prompt
  :functions inferior-lisp-proc
  :config
  (defun aorst/enable-fennel-eros ()
    "Enable mappings for evaluation overlays in fennel-mode."
    (local-set-key (kbd "C-M-x") #'aorst/fennel-eval-defun)
    (local-set-key (kbd "C-x C-e") #'aorst/fennel-eval-last-sexp))

  (defun aorst/fennel-eval-to-string (sexp)
    "Send SEXP to the inferior lisp process, return result as a string."
    (condition-case nil
        (let ((sexp (string-trim (substring-no-properties sexp)))
              (buf (get-buffer-create "*fennel-eval*"))
              (prompt inferior-lisp-prompt)
              (proc (inferior-lisp-proc)))
          (with-current-buffer buf
            (erase-buffer)
            (let ((comint-prompt-regexp prompt))
              (comint-redirect-send-command-to-process sexp buf proc t t))
            (accept-process-output proc)
            (ignore-errors ; apply font-locking if the result is balanced
              (setq-local delay-mode-hooks t)
              (setq delayed-mode-hooks nil)
              (check-parens)
              (fennel-mode)
              (font-lock-fontify-region (point-min) (point-max)))
            (let* ((contents (thread-last
                               (buffer-string)
                               (replace-regexp-in-string "^ +" "")
                               (string-replace "\n" " ")
                               (string-replace "\t" ", ")
                               string-trim))
                   (contents (if (string-empty-p contents)
                                 "#<no values>"
                               contents)))
              (message "%s" contents)
              contents)))
      (error "nil")))

  (defun aorst/fennel-eval-last-sexp ()
    "Eval last s-expression and display the result in an overlay."
    (interactive)
    (when (inferior-lisp-proc)
      (let ((sexp (buffer-substring (save-excursion (backward-sexp) (point)) (point))))
        (eros-eval-overlay
         (aorst/fennel-eval-to-string sexp)
         (point)))))

  (defun aorst/fennel-eval-defun ()
    "Eval defun and display the result in an overlay."
    (interactive)
    (when (inferior-lisp-proc)
      (eros-eval-overlay
       (aorst/fennel-eval-to-string (thing-at-point 'defun t))
       (save-excursion (end-of-defun) (point))))))


(use-package elisp-mode
  :straight nil
  :hook ((emacs-lisp-mode . eldoc-mode)
         (emacs-lisp-mode . common-lisp-modes-mode)
         (emacs-lisp-mode . aorst/emacs-lisp-setup))
  :config
  (defvar calculate-lisp-indent-last-sexp)

  (defun aorst/emacs-lisp-indent-function (indent-point state)
    "A replacement for `lisp-indent-function'.
Indents plists more sensibly. Adapted from DOOM Emacs:
https://github.com/hlissner/doom-emacs/blob/bf8495b4/modules/lang/emacs-lisp/autoload.el#L110"
    (let ((normal-indent (current-column))
          (orig-point (point))
          target)
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (cond ((and (elt state 2)
                  (or (not (looking-at-p "\\sw\\|\\s_"))
                      (eq (char-after) ?:)))
             (unless (> (save-excursion (forward-line 1) (point))
                        calculate-lisp-indent-last-sexp)
               (goto-char calculate-lisp-indent-last-sexp)
               (beginning-of-line)
               (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t))
             (backward-prefix-chars)
             (current-column))
            ((and (save-excursion
                    (goto-char indent-point)
                    (skip-syntax-forward " ")
                    (not (eq (char-after) ?:)))
                  (save-excursion
                    (goto-char orig-point)
                    (and (eq (char-after) ?:)
                         (eq (char-before) ?\()
                         (setq target (current-column)))))
             (save-excursion
               (move-to-column target t)
               target))
            ((let* ((function (buffer-substring (point) (progn (forward-sexp 1) (point))))
                    (method (or (function-get (intern-soft function) 'lisp-indent-function)
                                (get (intern-soft function) 'lisp-indent-hook))))
               (cond ((or (eq method 'defun)
                          (and (null method)
                               (> (length function) 3)
                               (string-match-p "\\`def" function)))
                      (lisp-indent-defform state indent-point))
                     ((integerp method)
                      (lisp-indent-specform method state indent-point normal-indent))
                     (method
                      (funcall method indent-point state))))))))

  (defun aorst/emacs-lisp-setup ()
    (setq-local lisp-indent-function
                #'aorst/emacs-lisp-indent-function)))


(use-package fennel-mode
  :commands (fennel-repl lisp-eval-string lisp-eval-last-sexp switch-to-lisp)
  :hook ((fennel-mode fennel-repl-mode) . common-lisp-modes-mode)
  :bind (:map fennel-mode-map
         ("C-c C-k" . aorst/eval-each-sexp)
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

  (defun aorst/eval-each-sexp (&optional arg)
    "Evaluate each s-expression in the buffer consequentially.
If prefix ARG specified, call `fennel-reload' function.  If
double prefix ARG specified call `fennel-reload' function and ask
for module name."
    (interactive "P")
    (if (symbolp arg)
        (save-excursion
          (save-restriction
            (goto-char (point-min))
            (while (save-excursion
                     (search-forward-regexp "[^[:space:]]." nil t))
              (forward-sexp)
              (lisp-eval-last-sexp))))
      (when fennel-mode-switch-to-repl-after-reload
        (switch-to-lisp t))
      (if (equal arg '(4))
          (funcall-interactively 'fennel-reload nil)
        (funcall-interactively 'fennel-reload t)))))


(use-package clojure-mode
  :hook ((clojure-mode
          clojurec-mode
          clojurescript-mode)
         . aorst/clojure-mode-setup)
  :config
  (defvar org-babel-default-header-args:clojure '((:results . "silent")))'

  (defun org-babel-execute:clojure (body params)
    "Evaluate a block of Clojure code with Babel."
    (lisp-eval-string body))

  (defun aorst/clojure-mode-setup ()
    "Setup Clojure buffer."
    (modify-syntax-entry ?# "w")
    (common-lisp-modes-mode)
    (flycheck-mode)))


(use-package inf-clojure
  :straight nil
  :requires (inf-lisp clojure-mode)
  :hook (inf-clojure-mode . common-lisp-modes-mode)
  :preface
  (require 'inf-lisp)
  (require 'clojure-mode)

  (defgroup inf-clojure ()
    "Support for interacting with Clojure via inferior process."
    :prefix "inf-clojure-"
    :group 'languages)

  (defcustom inf-clojure-program "clojure"
    "Path to the program used by `inf-clojure'"
    :tag "Clojure CLI"
    :type 'string
    :group 'inf-clojure)

  (defcustom inf-clojure-prompt "^[^>]+>>"
    "Prompt regex for inferior Clojure."
    :tag "Prompt regexp"
    :type 'string
    :group 'inf-clojure)

  (defun inf-clojure-indent-on-newline ()
    (interactive)
    (let (electric-indent-mode)
      (newline-and-indent)))

  (defun inf-clojure (&optional arg)
    "Run an inferior instance of `inf-clojure' inside Emacs."
    (interactive "p")
    (let ((buffer (comint-check-proc "inf-clojure")))
      (pop-to-buffer-same-window
       (if (or buffer (not (derived-mode-p 'inf-clojure-mode))
               (comint-check-proc (current-buffer)))
           (get-buffer-create (or buffer "*inf-clojure*"))
         (current-buffer)))
      (unless buffer
        (let* ((cmd (or (and (not (eq arg 1)) (read-from-minibuffer "Command: "))
                        inf-clojure-program))
               (cmdlist (if (fboundp #'split-string-shell-command)
                            (split-string-shell-command cmd)
                          ;; NOTE: Less accurate, may be problematic
                          (split-string cmd))))
          (apply 'make-comint-in-buffer "inf-clojure" buffer
                 (car cmdlist) nil (cdr cmdlist))
          (inf-clojure-mode)))))

  (define-derived-mode inf-clojure-mode inferior-lisp-mode "Inferior Clojure"
    "Major mode for `inf-clojure'.

\\<inf-clojure-mode-map>"
    (set (make-local-variable 'font-lock-defaults) '(clojure-font-lock-keywords t))
    (set (make-local-variable 'inferior-lisp-prompt) inf-clojure-prompt)
    (set (make-local-variable 'lisp-indent-function) 'clojure-indent-function)
    (set (make-local-variable 'lisp-doc-string-elt-property) 'clojure-doc-string-elt)
    (set (make-local-variable 'comment-end) "")
    (clojure-font-lock-setup)
    (make-local-variable 'completion-at-point-functions)
    (set-syntax-table clojure-mode-syntax-table)
    (add-hook 'paredit-mode-hook #'fennel-repl-paredit-setup nil t))

  (define-key inf-clojure-mode-map "C-j" 'inf-clojure-indent-on-newline)

  (provide 'inf-clojure))

(use-package cider
  :hook (((cider-repl-mode cider-mode) . eldoc-mode)
         (cider-repl-mode . common-lisp-modes-mode))
  :bind (:map cider-repl-mode-map
         ("C-c C-o" . cider-repl-clear-buffer))
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


(use-package sly
  :commands sly-symbol-completion-mode
  :hook (sly-mrepl-mode . common-lisp-modes-mode)
  :custom
  (inferior-lisp-program "sbcl")
  :config
  (sly-symbol-completion-mode -1))


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
  :bind (:map vterm-mode-map
         ("<insert>" . ignore)
         ("<f2>" . ignore))
  :custom
  (vterm-always-compile-module t)
  (vterm-environment '("VTERM=1")))


(use-package editorconfig
  :commands editorconfig-mode
  :init
  (editorconfig-mode 1))


(use-package flymake
  :straight nil
  :custom
  (flymake-fringe-indicator-position 'right-fringe))


(use-package flyspell
  :when (or (executable-find "ispell")
            (executable-find "aspell")
            (executable-find "hunspell"))
  :hook (((org-mode git-commit-mode markdown-mode) . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))


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
    (let ((text (pcase (or status flycheck-last-status-change)
                  (`not-checked "-/-")
                  (`no-checker "-")
                  (`running "*/*")
                  (`errored "!")
                  (`finished
                   (let-alist (flycheck-count-errors flycheck-current-errors)
                     (propertize (format "%s/%s" (or .error 0) (or .warning 0)))))
                  (`interrupted ".")
                  (`suspicious "?"))))
      (concat " " flycheck-mode-line-prefix ":" text)))

  (define-advice flycheck-may-use-echo-area-p (:override ())
    nil))


(use-package flycheck-package
  :hook ((emacs-lisp-mode . flycheck-mode)
         (emacs-lisp-mode . flycheck-package-setup)))


(use-package smartparens
  :commands (sp-use-paredit-bindings sp-local-pair sp-update-local-pairs)
  :hook (((common-lisp-modes-mode prog-mode) . smartparens-strict-mode)
         ((eval-expression-minibuffer-setup
           lisp-data-mode)
          . aorst/minibuffer-enable-sp))
  :bind (:map smartparens-mode-map
         ("C-M-q" . sp-indent-defun)
         ("M-q" . aorst/indent-or-fill-sexp)
         :map smartparens-strict-mode-map
         (";" . sp-comment))
  :custom
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  (sp-show-pair-delay 0)
  (sp-echo-match-when-invisible nil)
  :config
  (add-to-list 'sp-lisp-modes 'fennel-mode t)

  (require 'smartparens-config)

  (sp-use-paredit-bindings)
  (define-key smartparens-mode-map (kbd "M-r") 'sp-rewrap-sexp) ; needs to be set manually, because :bind section runs before :config

  (defun aorst/minibuffer-enable-sp ()
    "Enable `smartparens-strict-mode' in the minibuffer, during `eval-expression'."
    (setq-local comment-start ";")
    (sp-local-pair 'minibuffer-pairs "'" nil :actions nil)
    (sp-local-pair 'minibuffer-pairs "`" nil :actions nil)
    (sp-update-local-pairs 'minibuffer-pairs)
    (smartparens-strict-mode 1))

  (defun aorst/indent-or-fill-sexp ()
    "Indent s-expression or fill string/comment."
    (interactive)
    (let ((ppss (syntax-ppss)))
      (if (or (nth 3 ppss)
              (nth 4 ppss))
          (fill-paragraph)
        (indent-sexp)))))


(use-package undo-tree
  :commands global-undo-tree-mode
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


(use-package lsp-mode
  :hook (((c-mode
           c++-mode
           clojure-mode
           clojurec-mode
           clojurescript-mode)
          . lsp)
         (lsp-mode . yas-minor-mode))
  :custom
  ;; general settings
  (lsp-keymap-prefix "C-c l")
  (lsp-completion-provider :capf)
  (lsp-diagnostics-provider :auto)
  (lsp-session-file (expand-file-name ".lsp-session" user-emacs-directory))
  (lsp-log-io nil)
  (lsp-keep-workspace-alive nil)
  (lsp-idle-delay 0.05)

  ;; DAP
  (lsp-enable-dap-auto-configure nil)

  ;; UI
  (lsp-enable-links nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  (lsp-modeline-code-actions-enable nil)

  ;; semantic code features
  (lsp-enable-folding nil)
  (lsp-enable-indentation nil)
  (lsp-enable-semantic-highlighting nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-text-document-color nil)

  ;; completion
  (lsp-completion-show-kind nil)

  ;; lens
  (lsp-lens-enable t)
  (lsp-lens-place-position 'end-of-line))


(use-package lsp-java
  :requires lsp-mode
  :when (file-exists-p "/usr/lib/jvm/java-11-openjdk/bin/java")
  :custom
  (lsp-java-java-path "/usr/lib/jvm/java-11-openjdk/bin/java"))


(use-package treemacs
  :custom
  (treemacs-no-png-images t))


(use-package lsp-treemacs
  :custom
  (lsp-treemacs-theme "Iconless"))


(use-package project
  :functions (aorst/project-root-p
              aorst/project-find-root)
  :commands (project-buffers project-compile)
  :bind (:map project-prefix-map
         ("s" . aorst/project-save-some-buffers))
  :custom
  (project-compilation-buffer-name-function 'project-prefixed-buffer-name)
  :config
  (defvar aorst--project-root-markers
    '("Cargo.toml" "compile_commands.json" "compile_flags.txt" "project.clj" ".git" "deps.edn")
    "Files or directories that indicate the root of a project.")

  (defun aorst/project-root-p (path)
    "Check if current PATH has any of project root markers."
    (memq t (mapcar (lambda (file)
                      (file-exists-p (concat path file)))
                    aorst--project-root-markers)))

  (defun aorst/project-find-root (path)
    "Recursive search in PATH for root markers."
    (cond
     ((aorst/project-root-p path) (cons 'transient path))
     ((equal "/" path) nil)
     (t (aorst/project-find-root
         (file-name-directory
          (directory-file-name path))))))

  (add-to-list 'project-find-functions #'aorst/project-find-root)

  (define-advice project-compile (:around (fn &rest args))
    "Only ask to save project related buffers."
    (let* ((project-buffers (project-buffers (project-current)))
           (compilation-save-buffers-predicate
            (lambda () (memq (current-buffer) project-buffers))))
      (apply fn args)))

  (defun aorst/project-save-some-buffers (&optional arg)
    "Save some modified file-visiting buffers in current project.

Optional argument ARG (interactively, prefix argument) non-nil
means save all with no questions."
    (interactive "P")
    (let* ((project-buffers (project-buffers (project-current)))
           (pred (lambda () (memq (current-buffer) project-buffers))))
      (save-some-buffers arg pred))))


(use-package server
  :straight nil
  :config
  (unless (server-running-p)
    (server-start)))


(use-package separedit
  :hook (separedit-buffer-creation . aorst/separedit-header-line-setup)
  :bind (:map prog-mode-map
         ("C-c '" . separedit)
         :map separedit-mode-map
         ("C-c C-c" . separedit-commit)
         :map edit-indirect-mode-map
         ("C-c '" . separedit))
  :custom
  (separedit-default-mode 'gfm-mode)
  :config
  (defun aorst/separedit-header-line-setup ()
    (setq-local
     header-line-format
     (substitute-command-keys
      "Edit, then exit with `\\[separedit-commit]' or abort with `\\[edit-indirect-abort]'"))))


(use-package recentf
  :straight nil
  :custom
  (recentf-max-menu-items 50)
  :config
  (add-to-list 'recentf-exclude "\\.gpg\\")
  (recentf-mode))


(use-package dumb-jump
  :commands dumb-jump-xref-activate
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'completing-read)
  :init
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate))


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
  :hook (prog-mode . show-paren-mode)
  :custom
  (show-paren-delay 0)
  (show-paren-when-point-in-periphery t))


(use-package vc-hooks
  :straight nil
  :custom
  (vc-follow-symlinks t))


(use-package isayt
  :straight (:host gitlab
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
  :custom
  (compilation-scroll-output 'first-error)
  :config
  (aorst/add-compilation-error-syntax kaocha-tap
    "^not ok.*(\\([^:]*\\):\\([0-9]*\\))$"
    (1 "src/%s" "test/%s") 2)

  (aorst/add-compilation-error-syntax kaocha-fail
    ".*FAIL in.*(\\([^:]*\\):\\([0-9]*\\))$"
    (1 "src/%s" "test/%s") 2)

  (aorst/add-compilation-error-syntax clojure-reflection-warning
    "^Reflection warning,[[:space:]]*\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\).*$"
    (1 "src/%s" "test/%s") 2 3)

  (aorst/add-compilation-error-syntax clojure-syntax-error
    "^Syntax error macroexpanding at (\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)).$"
    (1 "src/%s" "test/%s") 2 3)

  (aorst/add-compilation-error-syntax lua-stacktrace
    "\\(?:^[[:space:]]+\\([^
:]+\\):\\([[:digit:]]+\\):[[:space:]]+in.+$\\)"
    1 2)

  (aorst/add-compilation-error-syntax fennel-compile-error
    "\\(?:^Compile error in[[:space:]]+\\([^:]+\\):\\([[:digit:]]+\\)$\\)"
    1 2))


(use-package isearch
  :straight nil
  :bind (:map isearch-mode-map
         ("<backspace>" . isearch-del-char)
         ("<left>" . aorst/isearch-backward-char)
         :map minibuffer-local-isearch-map
         ("<right>" . forward-char))
  :config
  (defun aorst/isearch-backward-char (&optional n)
    (interactive)
    (isearch-edit-string)
    (backward-char n)))


(use-package esh-mode
  :straight nil
  :custom
  (eshell-scroll-show-maximum-output nil))


(use-package dired
  :straight nil
  :custom
  (dired-listing-switches "-al --group-directories-first"))


(use-package comint
  :straight nil
  :custom
  (comint-scroll-show-maximum-output nil))


(use-package rect
  :straight nil
  :bind (("C-x r C-y" . aorst/yank-rectangle-add-lines))
  :config
  (defun aorst/yank-rectangle-add-lines ()
    (interactive "*")
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (save-restriction
      (narrow-to-region (point) (point))
      (yank-rectangle))))


(use-package jdecomp
  :hook (archive-mode . jdecomp-mode)
  :mode ("\\.class\\'" . jdecomp-mode)
  :custom
  (jdecomp-decompiler-type (cond ((file-exists-p aorst--cfr-path)
                                  'cfr)
                                 ((file-exists-p aorst--fernflower-path)
                                  'fernflower)
                                 (t jdecomp-decompiler-type)))
  (jdecomp-decompiler-paths `((cfr . ,aorst--cfr-path)
                              (fernflower . ,aorst--fernflower-path)))
  :init
  (defvar aorst--cfr-path
    (file-truename "~/.local/lib/cfr.jar")
    "Path to the cfr Java decompiler library.")

  (defvar aorst--fernflower-path
    (file-truename "~/.local/lib/fernflower.jar")
    "Path to the FernFlower library."))


(use-package lisp-butt-mode
  :hook (common-lisp-modes-mode . lisp-butt-mode))


(provide 'init)
;;; init.el ends here
