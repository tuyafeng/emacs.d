;;; init-markdown.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :defer t
  :config
  (setq markdown-command "pandoc")
  (setq markdown-split-window-direction 'right)
  (setq markdown-live-preview-window-function 'markdown-live-preview-window-eww))

(provide 'init-markdown)
;;; init-markdown.el ends here
