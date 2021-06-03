;;; treemacs-atom-theme.el --- Atom inspired theme for Treemacs -*- lexical-binding: t; buffer-read-only: t; no-byte-compile: t -*-
;;;
;;; Commentary:
;;; Simple Atom inspired theme for Treemacs package.
;;; Does not provide different kinds of icons for different file extensions.
;;; This file was automatically generated by `org-babel-tangle'.
;;; Do not change this file.  Treemacs theme config is located in README.org at `user-emacs-directory'
;;;
;;; Code:

(require 'treemacs)
(require 'all-the-icons)

(treemacs-create-theme "Atom"
  :config
  (progn
    (treemacs-create-icon
     :icon (format " %s\t%s\t"
                   (all-the-icons-octicon
                    "chevron-down"
                    :height 0.75
                    :v-adjust 0.1
                    :face '(:foreground unspecified :inherit shadow :slant normal :weight normal))
                   (all-the-icons-octicon
                    "file-directory"
                    :v-adjust 0
                    :face '(:foreground unspecified :inherit shadow :slant normal :weight normal)))
     :fallback (propertize "- " 'face 'shadow)
     :extensions (root-open))
    (treemacs-create-icon
     :icon (format " %s\t%s\t"
                   (all-the-icons-octicon
                    "chevron-right"
                    :height 0.75
                    :v-adjust 0.1
                    :face '(:foreground unspecified :inherit shadow :slant normal :weight normal))
                   (all-the-icons-octicon
                    "file-directory"
                    :v-adjust 0
                    :face '(:foreground unspecified :inherit shadow :slant normal :weight normal)))
     :fallback (propertize "+ " 'face 'shadow)
     :extensions (root-closed))
    (treemacs-create-icon
     :icon (format " %s\t%s\t"
                   (all-the-icons-octicon
                    "chevron-down"
                    :height 0.75
                    :v-adjust 0.1
                    :face '(:foreground unspecified :inherit shadow :slant normal :weight normal))
                   (all-the-icons-octicon
                    "file-directory"
                    :v-adjust 0
                    :face '(:foreground unspecified :inherit shadow :slant normal :weight normal)))
     :fallback (propertize "- " 'face 'shadow)
     :extensions (dir-open))
    (treemacs-create-icon
     :icon (format " %s\t%s\t"
                   (all-the-icons-octicon
                    "chevron-right"
                    :height 0.75
                    :v-adjust 0.1
                    :face '(:foreground unspecified :inherit shadow :slant normal :weight normal))
                   (all-the-icons-octicon
                    "file-directory"
                    :v-adjust 0
                    :face '(:foreground unspecified :inherit shadow :slant normal :weight normal)))
     :fallback (propertize "+ " 'face 'shadow)
     :extensions (dir-closed))
    (treemacs-create-icon
     :icon (format "  %s\t"
                   (all-the-icons-octicon
                    "chevron-down"
                    :height 0.75
                    :v-adjust 0.1
                    :face '(:foreground unspecified :inherit shadow :slant normal :weight normal)))
     :fallback (propertize "▾ " 'face 'font-lock-string-face)
     :extensions (tag-open))
    (treemacs-create-icon
     :icon (format "  %s\t"
                   (all-the-icons-octicon
                    "chevron-right"
                    :height 0.75
                    :v-adjust 0.1
                    :face '(:foreground unspecified :inherit shadow :slant normal :weight normal)))
     :fallback (propertize "▸ " 'face 'font-lock-string-face)
     :extensions (tag-closed))
    (treemacs-create-icon
     :icon (format "  %s\t"
                   (all-the-icons-octicon
                    "tag"
                    :height 0.9
                    :v-adjust 0
                    :face '(:foreground unspecified :inherit shadow :slant normal :weight normal)))
     :fallback ""
     :extensions (tag-leaf))
    (treemacs-create-icon
     :icon (format " %s\t"
                   (all-the-icons-octicon
                    "flame"
                    :v-adjust 0
                    :face '(:foreground unspecified :inherit shadow :slant normal :weight normal)))
     :fallback ""
     :extensions (error))
    (treemacs-create-icon
     :icon (format " %s\t"
                   (all-the-icons-octicon
                    "stop"
                    :v-adjust 0
                    :face '(:foreground unspecified :inherit shadow :slant normal :weight normal)))
     :fallback ""
     :extensions (warning))
    (treemacs-create-icon
     :icon (format " %s\t"
                   (all-the-icons-octicon
                    "info"
                    :height 0.75
                    :v-adjust 0.1
                    :face '(:foreground unspecified :inherit shadow :slant normal :weight normal)))
     :fallback ""
     :extensions (info))
    (treemacs-create-icon
     :icon (format "   %s\t"
                   (all-the-icons-octicon
                    "file-media"
                    :v-adjust 0
                    :face '(:foreground unspecified :inherit shadow :slant normal :weight normal)))
     :fallback ""
     :extensions ("png" "jpg" "jpeg" "gif" "ico" "tif" "tiff" "svg" "bmp"
                  "psd" "ai" "eps" "indd" "mov" "avi" "mp4" "webm" "mkv"
                  "wav" "mp3" "ogg" "midi"))
    (treemacs-create-icon
     :icon (format "   %s\t"
                   (all-the-icons-octicon
                    "file-code"
                    :v-adjust 0
                    :face '(:foreground unspecified :inherit shadow :slant normal :weight normal)))
     :fallback ""
     :extensions ("yml" "yaml" "sh" "zsh" "fish" "c" "h" "cpp" "cxx" "hpp"
                  "tpp" "cc" "hh" "hs" "lhs" "cabal" "py" "pyc" "rs" "el" "erl"
                  "elc" "clj" "cljs" "cljc" "ts" "tsx" "vue" "css" "html"
                  "htm" "dart" "java" "kt" "scala" "sbt" "go" "js" "jsx"
                  "hy" "json" "jl" "ex" "exs" "eex" "ml" "mli" "pp" "dockerfile"
                  "vagrantfile" "j2" "jinja2" "tex" "racket" "rkt" "rktl" "rktd"
                  "scrbl" "scribble" "plt" "makefile" "elm" "xml" "xsl" "rb"
                  "scss" "lua" "lisp" "scm" "sql" "toml" "nim" "pl" "pm" "perl"
                  "vimrc" "tridactylrc" "vimperatorrc" "ideavimrc" "vrapperrc"
                  "cask" "r" "re" "rei" "bashrc" "zshrc" "inputrc" "editorconfig"
                  "gitconfig" "gitignore" "gitmodules" "gitattributes" "conf" "lock"
                  "project" "fnl" "config"))
    (treemacs-create-icon
     :icon (format "   %s\t"
                   (all-the-icons-octicon
                    "book"
                    :v-adjust 0
                    :face '(:foreground unspecified :inherit shadow :slant normal :weight normal)))
     :fallback ""
     :extensions ("lrf" "lrx" "cbr" "cbz" "cb7" "cbt" "cba" "chm" "djvu"
                  "doc" "docx" "pdb" "pdb" "fb2" "xeb" "ceb" "inf" "azw"
                  "azw3" "kf8" "kfx" "lit" "prc" "mobi" "pkg" "opf" "txt"
                  "pdb" "ps" "rtf" "pdg" "xml" "tr2" "tr3" "oxps" "xps"))
    (treemacs-create-icon
     :icon (format "   %s\t" (all-the-icons-octicon
                              "file-text"
                              :v-adjust 0
                              :face '(:foreground unspecified :inherit shadow :slant normal :weight normal)))
     :fallback ""
     :extensions ("md" "markdown" "rst" "log" "org" "txt"
                  "contribute" "license" "readme" "changelog"))
    (treemacs-create-icon
     :icon (format "   %s\t" (all-the-icons-octicon
                              "file-binary"
                              :v-adjust 0
                              :face '(:foreground unspecified :inherit shadow :slant normal :weight normal)))
     :fallback ""
     :extensions ("exe" "dll" "obj" "so" "o" "out"))
    (treemacs-create-icon
     :icon (format "   %s\t" (all-the-icons-octicon
                              "file-pdf"
                              :v-adjust 0
                              :face '(:foreground unspecified :inherit shadow :slant normal :weight normal)))
     :fallback ""
     :extensions ("pdf"))
    (treemacs-create-icon
     :icon (format "   %s\t" (all-the-icons-octicon
                              "file-zip"
                              :v-adjust 0
                              :face '(:foreground unspecified :inherit shadow :slant normal :weight normal)))
     :fallback ""
     :extensions ("zip" "7z" "tar" "gz" "rar" "tgz"))
    (treemacs-create-icon
     :icon (format "   %s\t" (all-the-icons-octicon
                              "file-text"
                              :v-adjust 0
                              :face '(:foreground unspecified :inherit shadow :slant normal :weight normal)))
     :fallback ""
     :extensions (fallback))))

(provide 'treemacs-atom-theme)
;; treemacs-atom-theme.el ends here
