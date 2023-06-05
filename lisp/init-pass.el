;;; init-pass.el --- pass.el configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package epa-file
  :ensure nil
  :defer t
  :config
  (epa-file-enable)
  (setq epa-pinentry-mode 'loopback)
  (add-hook 'kill-emacs-hook (defun my/eaf-file--kill-gpg-agent ()
                               (shell-command "pkill gpg-agent"))))

(use-package pass
  :commands (pass my/pass-preheat)
  :config
  (defun my/pass-preheat ()
    "Preheat password storage"
    (interactive)
    (password-store-copy "test"))
  (setq pass-username-field "login"))

(defun my/pass-generate ()
  "Generate a password and copy it to the clipboard."
  (interactive)
  (shell-command "pwgen -cny 14 1 | tr -d '\n\t' | pbcopy && echo '0' || echo '1'"))

(provide 'init-pass)
;;; init-pass.el ends here
