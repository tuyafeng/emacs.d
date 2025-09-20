;;; init-treesit.el --- For treesit -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package treesit
  :ensure nil
  :config
  ;; `treesit-install-language-grammar`
  (dolist (lang-source
           '((kotlin . ("https://github.com/fwcd/tree-sitter-kotlin"))
             (java . ("https://github.com/tree-sitter/tree-sitter-java"))
             (python . ("https://github.com/tree-sitter/tree-sitter-python"))))
    (add-to-list 'treesit-language-source-alist lang-source)))

(provide 'init-treesit)
;;; init-treesit.el ends here
