;;; init-nov.el --- For reading EPUBs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package nov
  :defer t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq line-spacing 1)
  (setq nov-text-width t)
  (setq visual-fill-column-center-text t)
  (defun my/nov-mode-hook()
    (visual-line-mode 1)
    (visual-fill-column-mode 1))
  (add-hook 'nov-mode-hook #'my/nov-mode-hook))

(provide 'init-nov)
;;; init-nov.el ends here
