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

;; Reference: https://github.com/LuciusChen/.emacs.d/blob/main/lisp/init-auth.el
(defun my/pass-generate (&optional length no-symbols)
  "Interactively generate a random password.
If LENGTH is provided, it specifies the password length.
If NO-SYMBOLS is non-nil, the password will not contain symbols.
The default LENGTH is 16."
  (interactive
   (list (read-number "Password length: " 16)
         (not (y-or-n-p "Include symbols? "))))
  (let* ((chars (concat "abcdefghijklmnopqrstuvwxyz"
                        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                        "0123456789"
                        (unless no-symbols "!@#$%^&*()-_=+[]{}|;:,.<>?")))
         (password ""))
    (dotimes (_ length password)
      (setq password (concat password (string (elt chars (random (length chars)))))))
    (kill-new password)
    (message "Generated password: %s (Copied to clipboard)" password)
    password))

(with-eval-after-load 'launcher
  (defun my/pass-candidates ()
    (require 'password-store)
    (mapcar (lambda (entry)
              (list :id entry :title entry :data entry))
            (password-store-list)))

  (defun my/pass-copy (candidate)
    (require 'password-store)
    (password-store-copy (plist-get candidate :data))
    (message "Password copied.")
    nil)

  (add-to-list 'launcher-tools
               `("pass" :name "Pass"
                        :icon ,(when (featurep 'nerd-icons)
                                 (nerd-icons-faicon "nf-fa-key"))
                        :candidates my/pass-candidates
                        :submit my/pass-copy)))

(provide 'init-pass)
;;; init-pass.el ends here
