;;; init.el --- Emacs configuration of Yafeng Tu -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (version< emacs-version "25")
  (warn "This configuration needs Emacs trunk, but this is %s!" emacs-version))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(require 'init-core)
(require 'init-package)

(require 'init-themes)
(require 'init-gui-frames)
(require 'init-nerd-icons)
(require 'init-windows)
(require 'init-minibuffer)
(require 'init-mode-line)

(require 'init-editor)

(require 'init-recentf)
(require 'init-project)
(require 'init-whitespace)
(require 'init-ibuffer)
(require 'init-isearch)
(require 'init-eww)
(require 'init-dired)
(require 'init-utils)

(require 'init-corfu)
(require 'init-vterm)
(require 'init-eglot)
(require 'init-git)

(require 'init-org)
(require 'init-python)
(require 'init-markdown)
(require 'init-lisp)
(require 'init-flutter)
(require 'init-csv)

(require 'init-telega)
(require 'init-pass)
(require 'init-mpv)
(require 'init-google-translate)
(require 'init-yasnippet)

(provide 'init)
;;; init.el ends here
