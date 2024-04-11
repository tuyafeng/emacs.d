;;; init-vterm.el --- vterm.el configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vterm
  :defer t
  :init
  (setq vterm-shell "zsh")
  :config
  (setq vterm-always-compile-module t))

(provide 'init-vterm)
;;; init-vterm.el ends here
