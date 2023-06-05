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
  (before-save . (lambda ()
                   (when (derived-mode-p 'prog-mode 'markdown-mode 'org-mode)
                     (delete-trailing-whitespace))))
  :config
  (setq whitespace-style '(face trailing)))

(provide 'init-whitespace)
;;; init-whitespace.el ends here
