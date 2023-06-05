;;; init-mode-line.el --- Mode line configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(line-number-mode t)
(column-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(setcar mode-line-position-column-line-format " %l:%c")
(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info mode-line-modified mode-line-remote
                "  "
                mode-line-buffer-identification
                "  "
                mode-line-modes
                mode-line-misc-info
                (vc-mode vc-mode)
                "  "
                mode-line-position
                mode-line-end-spaces))

(use-package keycast
  :hook (after-init . keycast-mode)
  :config
  (setq keycast-mode-line-format "%5s%K%C%r")
  (setq keycast-mode-line-remove-tail-elements nil)
  (setq keycast-mode-line-insert-after 'mode-line-end-spaces))

(provide 'init-mode-line)
;;; init-mode-line.el ends here
