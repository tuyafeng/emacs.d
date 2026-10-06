;;; init.el --- Emacs configuration of Yafeng Tu -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (< emacs-major-version 31)
  (error "Emacs 31 or higher is required"))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-core)
(require 'init-package)

(require 'init-themes)
(require 'init-gui-frames)
(require 'init-nerd-icons)
(require 'init-windows)
(require 'init-minibuffer)
(require 'init-mode-line)
(require 'init-tab-bar)

(require 'init-editor)

(require 'init-recentf)
(require 'init-project)
(require 'init-whitespace)
(require 'init-ibuffer)
(require 'init-isearch)
(require 'init-eww)
(require 'init-dired)

(require 'init-corfu)
(unless (eq system-type 'windows-nt)
  (require 'init-vterm))
(require 'init-treesit)
(require 'init-git)

(require 'init-org)
(require 'init-markdown)
(require 'init-csv)

(require 'init-telega)
(require 'init-pass)
(require 'init-mpv)
(require 'init-tempel)
(require 'init-android)
(require 'init-gpt)
(require 'init-nov)
(when (eq system-type 'darwin)
  (require 'init-caffeinate))

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
