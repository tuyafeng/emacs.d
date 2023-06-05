;;; init-eww.el --- EWW configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eww
  :ensure nil
  :defer t
  :config
  (setq shr-width 100)
  (setq eww-search-prefix "https://html.duckduckgo.com/html?q="))

(use-package mb-url
  :defer t
  :commands (mb-url-http-around-advice)
  :init
  (setq mb-url-http-backend 'mb-url-http-curl
        mb-url-http-curl-switches `("--max-time" "20" "-x" ,"socks5h://127.0.0.1:1090"))
  (advice-add 'url-http :around 'mb-url-http-around-advice))

(provide 'init-eww)
;;; init-eww.el ends here
