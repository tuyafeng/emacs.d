;;; init-neotree.el --- neotree.el configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package neotree
  :commands
  (neotree-toggle)
  :bind
  ("<f8>" . neotree-toggle)
  :config
  (setq neo-window-width 30)
  (setq neo-theme 'arrow))

(provide 'init-neotree)
;;; init-neotree.el ends here
