;;; init-nerd-icons.el --- For icons -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Don't forget: M-x nerd-icons-install-fonts
(use-package nerd-icons
  :config
  (add-hook 'after-init-hook 'nerd-icons-set-font))

(provide 'init-nerd-icons)
;;; init-nerd-icons.el ends here
