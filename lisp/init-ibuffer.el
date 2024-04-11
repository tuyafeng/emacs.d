;;; init-ibuffer.el --- ibuffer configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ibuffer
  :ensure nil
  :bind
  ("C-x C-b" . 'ibuffer)
  :commands (ibuffer)
  :config
  (setq ibuffer-use-other-window t))

(eq (next-window) (selected-window))

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
