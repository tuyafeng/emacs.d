;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :config
  (add-to-list 'major-mode-remap-alist
               '(python-mode . python-ts-mode)))

(provide 'init-python)
;;; init-python.el ends here
