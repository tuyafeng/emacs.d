;;; init-ibuffer.el --- ibuffer configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(global-set-key (kbd "s-w") 'kill-current-buffer)

(defun my/kill-other-buffers ()
  "Kill other buffers."
  (interactive)
  (dolist (buffer (delq (current-buffer) (buffer-list)))
    (kill-buffer buffer)))

(use-package nerd-icons-ibuffer
  :after (ibuffer nerd-icons)
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
