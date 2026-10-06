;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit
  :commands (magit my/magit-add-all-and-commit)
  :config
  (defun my/magit-add-all-and-commit ()
    "Stage all changes and open the Magit commit buffer."
    (interactive)
    (if-let* ((default-directory (magit-toplevel)))
        (progn
          (magit-call-git "add" "-A")
          (if (magit-anything-staged-p)
              (magit-commit-create)
            (message "No changes to commit.")))
      (message "Not inside a Git repository.")))
  :custom
  (magit-define-global-key-bindings nil))

(provide 'init-git)
;;; init-git.el ends here
