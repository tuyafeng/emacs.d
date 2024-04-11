;;; init-package.el --- Settings and helpers for package.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)

(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(let ((package-selected-packages '(use-package diminish)))
  (when (cl-find-if-not #'package-installed-p package-selected-packages)
    (package-refresh-contents)
    (mapc #'package-install package-selected-packages)))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t)
  (setq use-package-compute-statistics 1))

(use-package benchmark-init
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(require 'bind-key)
(require 'diminish)

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq backup-directory-alist
        `((".*" ,(no-littering-expand-var-file-name "auto-save/"))))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(use-package quelpa-use-package
  :init
  (setq quelpa-update-melpa-p nil
        quelpa-checkout-melpa-p nil
        quelpa-git-clone-depth 1))

(provide 'init-package)
;;; init-package.el ends here
