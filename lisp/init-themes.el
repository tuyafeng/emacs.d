;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package modus-themes
  :config
  ;; Make the mode line borderless
  (setq modus-themes-common-palette-overrides
        '(;; Make the mode line borderless
          (border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)
          ;; Set the mode line with purple style
          (bg-mode-line-active bg-lavender)
          (fg-mode-line-active fg-main)))
  (load-theme 'modus-operandi :no-confirm)
  (when (eq system-type 'darwin)
    (defun my/apply-theme (appearance)
      "Load theme, taking current system APPEARANCE into consideration."
      (mapc #'disable-theme custom-enabled-themes)
      (pcase appearance
        ('light (load-theme 'modus-operandi :no-confirm))
        ('dark (load-theme 'modus-vivendi :no-confirm))))
    (add-hook 'ns-system-appearance-change-functions #'my/apply-theme))
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(provide 'init-themes)
;;; init-themes.el ends here
