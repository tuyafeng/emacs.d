;;; init-vterm.el --- vterm.el configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (eq system-type 'darwin)
  (use-package vterm
    :commands (vterm vterm-other-window)
    :init
    (setq vterm-shell "zsh")
    :config
    (setq vterm-always-compile-module t)
    (defun my/vterm-mode-hook ()
      ;; Hide line numbers
      (display-line-numbers-mode -1))
    (add-hook 'vterm-mode-hook #'my/vterm-mode-hook)))

(provide 'init-vterm)
;;; init-vterm.el ends here
