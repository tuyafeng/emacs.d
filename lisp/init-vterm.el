;;; init-vterm.el --- vterm.el configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vterm
  :defer t
  :init
  (setq vterm-shell "zsh")
  :bind
  (:map vterm-mode-map
        ("C-c d v" . my/vterm-cd-to-visible-dir)
        ("C-c d p" . my/vterm-cd-to-project-root))
  :config
  (setq vterm-always-compile-module t)
  (defun my/vterm-cd-to-visible-dir ()
    "cd in vterm to directory of a visible non-vterm buffer."
    (interactive)
    (let* ((win (cl-find-if
                 (lambda (w)
                   (with-current-buffer (window-buffer w)
                     (not (eq major-mode 'vterm-mode))))
                 (window-list)))
           (dir (when win
                  (with-current-buffer (window-buffer win)
                    (cond
                     ;; dired buffer: use its directory
                     ((eq major-mode 'dired-mode)
                      (dired-current-directory))
                     ;; file-visiting buffer: use file's dir
                     (buffer-file-name
                      (file-name-directory buffer-file-name))
                     ;; fallback
                     (t default-directory))))))
      (if dir
          (my/vterm--cd dir)
        (message "No visible buffer with directory found."))))
  (defun my/vterm-cd-to-project-root ()
    "cd in vterm to current project root."
    (interactive)
    (my/vterm--cd (project-root (project-current t))))
  (defun my/vterm--cd (dir)
    "In vterm, send `cd DIR`."
    (unless (derived-mode-p 'vterm-mode)
      (user-error "Not in vterm"))
    (let ((dir (and dir (expand-file-name dir))))
      (unless dir
        (user-error "No directory"))
      (vterm-send-string (format "cd %s" (shell-quote-argument dir)))
      (vterm-send-return)))
  (defun my/vterm--disable-hl-line ()
    "Disable `global-hl-line-mode' in vterm buffers."
    (setq-local global-hl-line-mode nil))
  (add-hook 'vterm-mode-hook #'my/vterm--disable-hl-line))

(provide 'init-vterm)
;;; init-vterm.el ends here
