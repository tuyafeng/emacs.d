;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit
  :commands (magit)
  :custom
  (magit-define-global-key-bindings nil))

(provide 'init-git)
;;; init-git.el ends here
