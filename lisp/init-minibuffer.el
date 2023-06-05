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

(use-package embark
  :commands (embark-act)
  :config
  (setq prefix-help-command 'embark-prefix-help-command)
  :bind
  ("C-;" . 'embark-act))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
