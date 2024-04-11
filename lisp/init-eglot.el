;;; init-eglot.el --- LSP support via eglot -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :defer t
  :hook
  (python-mode . eglot-ensure)
  (dart-mode . eglot-ensure)
  (sh-mode . eglot-ensure)
  (html-mode . eglot-ensure)
  (css-mode . eglot-ensure)
  (js-mode . eglot-ensure)
  (js-json-mode . eglot-ensure)
  :config
  (setq read-process-output-max (* 1024 1024))
  (push :documentHighlightProvider eglot-ignored-server-capabilities)
  (setq eldoc-echo-area-use-multiline-p 5)
  (setq eldoc-echo-area-display-truncation-message nil))

(provide 'init-eglot)
;;; init-eglot.el ends here
