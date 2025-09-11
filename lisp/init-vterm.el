;;; init-vterm.el --- vterm.el configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vterm
  :defer t
  :init
  (setq vterm-shell "zsh")
  :bind
  (:map vterm-mode-map
        ("C-c d" . my/vterm-cd-to-visible-dired))
  :config
  (setq vterm-always-compile-module t)
  (defun my/vterm-cd-to-visible-dired ()
    "Send `cd` to vterm, using the directory of the visible dired buffer
in the current frame."
    (interactive)
    (let* ((win (cl-find-if
                 (lambda (w)
                   (with-current-buffer (window-buffer w)
                     (eq major-mode 'dired-mode)))
                 (window-list)))
           (dir (when win
                  (with-current-buffer (window-buffer win)
                    default-directory))))
      (if dir
          (progn
            (vterm-send-string (concat "cd " (shell-quote-argument (expand-file-name dir))))
            (vterm-send-return))
        (message "No visible dired window found.")))))

(provide 'init-vterm)
;;; init-vterm.el ends here
