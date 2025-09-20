;;; init-vterm.el --- vterm.el configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vterm
  :defer t
  :init
  (setq vterm-shell "zsh")
  :bind
  (:map vterm-mode-map
        ("C-c C-d" . my/vterm-cd-to-visible-dir))
  :config
  (setq vterm-always-compile-module t)
  (defun my/vterm-cd-to-visible-dir ()
    "Send `cd` to vterm, using dired path if buffer is dired-mode, else file's directory."
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
          (progn
            (vterm-send-string (concat "cd " (shell-quote-argument
                                              (expand-file-name dir))))
            (vterm-send-return))
        (message "No visible buffer with directory found."))))
  (defun my/vterm--disable-hl-line ()
    "Disable `global-hl-line-mode' in vterm buffers."
    (setq-local global-hl-line-mode nil))
  (add-hook 'vterm-mode-hook #'my/vterm--disable-hl-line))

(provide 'init-vterm)
;;; init-vterm.el ends here
