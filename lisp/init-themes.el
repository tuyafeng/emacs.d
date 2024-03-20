;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package modus-themes
  :config
  ;; Make the mode line borderless
  (setq modus-themes-common-palette-overrides
      '((border-mode-line-active bg-mode-line-active)
        (border-mode-line-inactive bg-mode-line-inactive)))
  ;; (load-theme 'modus-vivendi :no-confirm)
  (load-theme 'modus-operandi :no-confirm)
  :bind ("<f5>" . modus-themes-toggle))

(when (eq system-type 'darwin)
  (defun my/apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'modus-operandi :no-confirm))
      ('dark (load-theme 'modus-vivendi :no-confirm))))
  (add-hook 'ns-system-appearance-change-functions #'my/apply-theme))

(provide 'init-themes)
;;; init-themes.el ends here
