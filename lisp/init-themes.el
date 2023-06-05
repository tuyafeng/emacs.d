;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package modus-themes
  :init
  (setq modus-themes-mode-line '(borderless (padding . 4) (height . 0.9)))
  ;; (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi) ;; OR (modus-themes-load-operandi)
  :bind ("<f5>" . modus-themes-toggle))

(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (modus-themes-load-operandi))
    ('dark (modus-themes-load-vivendi))))

(when IS-MAC
  (add-hook 'ns-system-appearance-change-functions #'my/apply-theme))

(provide 'init-themes)
;;; init-themes.el ends here
