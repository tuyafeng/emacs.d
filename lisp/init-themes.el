;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(mapc #'disable-theme custom-enabled-themes)

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


  ;; Disable other themes before loading the theme
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'modus-operandi :no-confirm)

  (when (eq system-type 'darwin)
    (defun my/apply-theme (appearance)
      "Load theme, taking current system APPEARANCE into consideration."
      (mapc #'disable-theme custom-enabled-themes)
      (pcase appearance
        ('light (load-theme 'modus-operandi :no-confirm))
        ('dark (load-theme 'modus-vivendi :no-confirm))))
    (add-hook 'ns-system-appearance-change-functions #'my/apply-theme))

  ;; Make the fill-column-indicator thinner
  ;; Reference: https://protesilaos.com/emacs/modus-themes#h:2a602816-bc1b-45bf-9675-4cbbd7bf6cab
  (modus-themes-with-colors
    (custom-set-faces
     `(fill-column-indicator ((,c :height 0.1)))))

  (setq modus-themes-to-toggle '(modus-operandi modus-vivendi))
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(provide 'init-themes)
;;; init-themes.el ends here
