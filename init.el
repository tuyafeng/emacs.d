;;; init.el --- Emacs configuration of Yafeng Tu -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (version< emacs-version "25")
  (warn "This configuration needs Emacs trunk, but this is %s!" emacs-version))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD     (or IS-MAC (eq system-type 'berkeley-unix)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(require 'init-core)
(require 'init-package)

(require 'init-gui-frames)
(require 'init-themes)
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
(require 'init-neotree)
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

(provide 'init)
;;; init.el ends here
