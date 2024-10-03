;;; init-whitespace.el --- Special handling for whitespace -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package whitespace
  :ensure nil
  :diminish whitespace-mode
  :hook
  ((prog-mode markdown-mode org-mode) . whitespace-mode)
  :config
  (setq whitespace-style
        '(face             ; visualize things below:
          empty            ; empty lines at beginning/end of buffer
          space-before-tab ; spaces before tab
          trailing         ; trailing blanks
          tabs             ; tabs (show by face)
          tab-mark         ; tabs (show by symbol)
          ))
  (defun my/whitespace-before-save-hook()
    (when (derived-mode-p 'prog-mode 'markdown-mode 'org-mode)
      (delete-trailing-whitespace)))
  (add-hook 'before-save-hook #'my/whitespace-before-save-hook))

(provide 'init-whitespace)
;;; init-whitespace.el ends here
