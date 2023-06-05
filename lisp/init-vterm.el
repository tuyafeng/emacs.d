;;; init-vterm.el --- vterm.el configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vterm
  :when (eq system-type 'darwin)
  :commands (vterm vterm-other-window)
  :init
  (setq vterm-shell "zsh")
  :config
  (setq vterm-always-compile-module t))

(provide 'init-vterm)
;;; init-vterm.el ends here
