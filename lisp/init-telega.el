;;; init-telega.el --- telega.el configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package telega
  :commands (telega)
  :config
  (unless (display-graphic-p) (setq telega-use-images nil))
  (setq telega-server-libs-prefix (expand-file-name "~/.local/tdlib"))
  (setq telega-proxies
        (list '(:server "127.0.0.1" :port 1090 :enable t
                        :type (:@type "proxyTypeSocks5"))))
  (setq telega-chat-show-avatars nil
        telega-root-show-avatars nil
        telega-user-show-avatars nil
        telega-active-locations-show-avatars nil
        telega-company-username-show-avatars nil)
  (telega-mode-line-mode 1))

(provide 'init-telega)
;;; init-telega.el ends here
