;;; center-view.el --- Center View Mode for Emacs
;;; Commentary:
;;; A minor mode that makes text in current frame
;;; centered horizontally.  This is useful when
;;; writing text, and working with large windows.
;;; Code:

(defvar center-view-mode-hook nil)
(defvar center-view-mode nil)
(defvar center-view-extra-space 10)

(make-variable-buffer-local
 (progn
   (defvar center-view--margin-size 0)
   (defvar center-view--buffer-window nil)))

;;;###autoload
(define-minor-mode center-view-mode
  "Toggle Center View Mode.

This mode makes buffer contents centered."
  :lighter " center-view"
  (if center-view-mode
      (progn
        (setq center-view--buffer-window (get-buffer-window))
        (add-hook 'window-size-change-functions
                  'center-view)
        (center-view))
    (progn
      (remove-hook 'window-size-change-functions
                   'center-view)
      (set-window-margins nil 0 0))))

;;;###autoload
(defun center-view (&optional _)
  "Main centering function.

If current window is wider than `fill-column' + `center-view-extra-space'
active buffer will be displayed centered.  This function accepts optional
frame argument, however it is done only to please `window-size-change-functions'
format."
  (if (and (>= (window-total-width)
               (+ fill-column center-view-extra-space))
           center-view--buffer-window)
      (let ((center-view--margin-size
             (/ (- (window-total-width center-view--buffer-window)
                   (+ fill-column center-view-extra-space))
                2)))
        (set-window-margins center-view--buffer-window
                            center-view--margin-size
                            center-view--margin-size))))

(provide 'center-view-mode)
;;; center-view.el ends here
