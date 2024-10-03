;;; init-web.el --- Front-end coding -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package web-mode
  :defer t
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(provide 'init-web)
;;; init-web.el ends here
