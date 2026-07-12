;;; init-nov.el --- For reading EPUBs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package nov
  :defer t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width 80)
  (define-key nov-mode-map (kbd "RET") #'nov-scroll-up)
  (define-key nov-mode-map (kbd "DEL") #'nov-scroll-down)
  (defun my/nov-mode-hook()
    (setq-local line-spacing 1)
    (setq-local global-hl-line-mode nil))
  (add-hook 'nov-mode-hook #'my/nov-mode-hook))

(provide 'init-nov)
;;; init-nov.el ends here
