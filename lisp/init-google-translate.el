;;; init-google-translate.el --- Google translate support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package google-translate
  :defer t
  :init
  (setq google-translate-translation-directions-alist
        '(("zh-CN" . "en") ("en" . "zh-CN")))
  :bind
  ("C-c t" . 'google-translate-smooth-translate))

(provide 'init-google-translate)
;;; init-google-translate.el ends here
