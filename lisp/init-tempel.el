;;; init-tempel.el --- TempEL -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package tempel
  :bind
  (("C-c t c" . tempel-complete)
   ("C-c t i" . tempel-insert)
   (:map tempel-map
        ("<tab>"   . tempel-next)
        ("<backtab>" . tempel-previous))))

(provide 'init-tempel)
;;; init-tempel.el ends here
