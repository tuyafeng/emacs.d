;;; init-isearch.el --- isearch configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package isearch
  :ensure nil
  :defer t
  :config
  (setq isearch-allow-scroll t)
  :bind (:map isearch-mode-map
              ([remap isearch-delete-char] . isearch-del-char))
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format nil)
  (lazy-count-suffix-format " [%s/%s]"))

(defun my/isearch-forward ()
  "Start isearch. If region is active, use it as initial search string."
  (interactive)
  (let ((text (when (use-region-p)
                (buffer-substring-no-properties
                 (region-beginning) (region-end)))))
    (deactivate-mark)
    (isearch-forward nil 1)
    (when text
      (isearch-yank-string text))))

(global-set-key (kbd "C-s") #'my/isearch-forward)

(defun my/isearch-backward ()
  "Start backward isearch. If region is active, use it as initial search string."
  (interactive)
  (let ((text (when (use-region-p)
                (buffer-substring-no-properties
                 (region-beginning) (region-end)))))
    (deactivate-mark)
    (isearch-backward nil 1)
    (when text
      (isearch-yank-string text))))

(global-set-key (kbd "C-r") #'my/isearch-backward)

(provide 'init-isearch)
;;; init-isearch.el ends here
