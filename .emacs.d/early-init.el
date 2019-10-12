(defvar my--gc-cons-threshold gc-cons-threshold)
(defvar my--gc-cons-percentage gc-cons-percentage)
(defvar my--file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      inhibit-compacting-font-caches t
      message-log-max 16384
      file-name-handler-alist nil)

(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold my--gc-cons-threshold
                  gc-cons-percentage my--gc-cons-percentage
                  file-name-handler-alist my--file-name-handler-alist)))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(defvar package--init-file-ensured)
(setq package-enable-at-startup nil
      package--init-file-ensured t)
