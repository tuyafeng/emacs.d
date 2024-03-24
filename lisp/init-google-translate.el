;;; init-google-translate.el --- Google translate support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package google-translate
  :commands (google-translate-smooth-translate)
  :bind
  ("C-c t" . 'google-translate-smooth-translate)
  :init
  (setq google-translate-translation-directions-alist
        '(("zh-CN" . "en") ("en" . "zh-CN"))))

(provide 'init-google-translate)
;;; init-google-translate.el ends here
