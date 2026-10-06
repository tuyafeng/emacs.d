;;; init-treesit.el --- For treesit -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package treesit
  :ensure nil
  :config
  (setq treesit-enabled-modes t)
  (setq js-indent-level 2)
  (setq css-indent-offset 2))

(provide 'init-treesit)
;;; init-treesit.el ends here
