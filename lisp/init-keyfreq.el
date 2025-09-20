;;; init-keyfreq-bar.el --- Keyfreq configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package keyfreq
  :hook
  (after-init . keyfreq-mode)
  (after-init . keyfreq-autosave-mode)
  :config
  (setq keyfreq-excluded-commands
        '(self-insert-command
          delete-backward-char
          forward-char
          backward-char
          previous-line
          next-line
          vertico-previous
          vertico-next
          ultra-scroll
          pixel-scroll-precision
          mwheel-scroll
          mouse-set-point
          mouse-set-region
          mouse-drag-region
          )))

(provide 'init-keyfreq)
;;; init-keyfreq.el ends here
