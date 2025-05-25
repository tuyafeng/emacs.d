;;; init.el --- Emacs configuration of Yafeng Tu -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(let ((minver "26.1"))
  (when (version< emacs-version minver)
    (error "Emacs v%s or higher is required" minver)))

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
(require 'init-eglot)
(require 'init-git)

(require 'init-org)
(require 'init-python)
(require 'init-markdown)
(require 'init-lisp)
(require 'init-flutter)
(require 'init-web)
(require 'init-csv)

(require 'init-telega)
(require 'init-pass)
(require 'init-mpv)
(require 'init-google-translate)
(require 'init-yasnippet)
(require 'init-nov)
(require 'init-android)
(require 'init-yaml)
(require 'init-elfeed)
(require 'init-speed-type)
(require 'init-gpt)

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
