;;; init-pass.el --- pass.el configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package epa-file
  :ensure nil
  :defer t
  :config
  (epa-file-enable)
  (setq epa-pinentry-mode 'loopback)
  (defun my/epa-file-kill-emacs-hook()
    (shell-command "pkill gpg-agent"))
  (add-hook 'kill-emacs-hook #'my/epa-file-kill-emacs-hook))

(use-package pass
  :commands (pass my/pass-preheat)
  :config
  (defun my/pass-preheat ()
    "Preheat password storage."
    (interactive)
    (password-store-copy "test"))
  (setq pass-username-field "login"))

(when (and (eq system-type 'darwin)
           (executable-find "pwgen"))
  (defun my/pass-generate ()
    "Generate a password and copy it to the clipboard."
    (interactive)
    (shell-command "pwgen -cny 14 1 | tr -d '\n\t' | pbcopy && echo '0' || echo '1'")))

(provide 'init-pass)
;;; init-pass.el ends here
