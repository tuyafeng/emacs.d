;;; init-csv.el --- CSV files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package csv-mode
  :defer t
  :config
  (defun my/toggle-csv-align-mode ()
    "Toggle csv-align-mode for CSV files."
    (interactive)
    (if (bound-and-true-p csv-align-mode)
        (progn
          (csv-align-mode -1))
      (csv-align-mode 1)))
  (define-key csv-mode-map (kbd "C-c C-a") #'my/toggle-csv-align-mode))

(provide 'init-csv)
;;; init-csv.el ends here
