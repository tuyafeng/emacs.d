;;; init-whitespace.el --- Special handling for whitespace -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package whitespace
  :ensure nil
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :hook
  ((prog-mode markdown-mode org-mode) . whitespace-mode)
  :config
  (setq whitespace-style '(face trailing))
  (defun my/whitespace-before-save-hook()
    (when (derived-mode-p 'prog-mode 'markdown-mode 'org-mode)
      (delete-trailing-whitespace)))
  (add-hook 'before-save-hook #'my/whitespace-before-save-hook))

(provide 'init-whitespace)
;;; init-whitespace.el ends here
