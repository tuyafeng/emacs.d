;;; init-markdown.el --- Markdown support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package markdown-ts-mode
  :ensure nil
  :mode ("\\.md\\'" . markdown-ts-mode)
  :bind (:map markdown-ts-mode-map
              ("C-c v" . my/markdown-preview-eww))
  :config
  (defun my/markdown-preview-eww ()
    "Preview current Markdown buffer in EWW using Pandoc."
    (interactive)
    (unless buffer-file-name
      (user-error "Buffer is not visiting a file"))
    (let* ((default-directory (file-name-directory buffer-file-name))
           (html-file (make-temp-file "markdown-preview-" nil ".html"))
           (status
            (call-process-region
             (point-min) (point-max)
             "pandoc" nil nil nil
             "-f" "markdown"
             "-t" "html"
             "--standalone"
             "--resource-path" default-directory
             "-o" html-file)))
      (if (zerop status)
          (eww-open-file html-file)
        (user-error "Pandoc failed with exit code %s" status)))))

(provide 'init-markdown)
;;; init-markdown.el ends here
