;;; init-csv.el --- CSV files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package csv-mode
  :defer t
  :hook ((csv-mode . csv-align-mode)
         (csv-mode . read-only-mode)))

(provide 'init-csv)
;;; init-csv.el ends here
