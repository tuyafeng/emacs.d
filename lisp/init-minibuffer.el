;;; init-minibuffer.el --- Config for minibuffer completion -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package vertico
  :hook (after-init . vertico-mode))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package orderless
  :config
  (setq completion-styles '(orderless)))

(use-package nerd-icons-completion
  :after (marginalia nerd-icons)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
