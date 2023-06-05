;;; init-recentf.el --- Settings for tracking recent files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-filename-handlers
        (append '(abbreviate-file-name) recentf-filename-handlers))
  (setq recentf-max-menu-items 30)
  (add-to-list 'recentf-exclude "/Applications/.*")
  (add-to-list 'recentf-exclude (expand-file-name "elpa" user-emacs-directory))
  :bind
  ("C-x C-r" . 'recentf-open-files))

(provide 'init-recentf)
;;; init-recentf.el ends here
