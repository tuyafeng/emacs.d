;;; init-flutter.el --- Flutter development environment -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dart-mode
  :defer t)

(use-package flutter
  :defer t
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload)))

(provide 'init-flutter)
;;; init-flutter.el ends here
