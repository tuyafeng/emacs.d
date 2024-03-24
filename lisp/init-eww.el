;;; init-eww.el --- EWW configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eww
  :ensure nil
  :defer t
  :config
  (setq shr-width 100)
  (setq eww-search-prefix "https://html.duckduckgo.com/html?q=")

  (defun my/eww-open-url-at-point-with-external-browser ()
    "Open the URL at point with an external browser."
    (interactive)
    (let ((links (eww-links-at-point)))
      (when links
        (let ((url (completing-read "Select URL:" links)))
          (eww-browse-with-external-browser url)))))
  (define-key eww-link-keymap (kbd "o") 'my/eww-open-url-at-point-with-external-browser)

  ;; Reference: https://emacs.stackexchange.com/a/38639
  (defun my/eww-toggle-images ()
    "Toggle whether images are loaded and reload the current page from cache."
    (interactive)
    (setq-local shr-inhibit-images (not shr-inhibit-images))
    (eww-reload t)
    (message "Images are now %s"
             (if shr-inhibit-images "off" "on")))

  (define-key eww-mode-map (kbd "I") #'my/eww-toggle-images)
  (define-key eww-link-keymap (kbd "I") #'my/eww-toggle-images)

  ;; Minimal rendering by default
  (setq-default shr-inhibit-images t)   ; toggle with `I`
  (setq-default shr-use-fonts nil)      ; toggle with `F`

  (define-key eww-mode-map "[" 'eww-back-url)
  (define-key eww-mode-map "]" 'eww-forward-url))

(use-package mb-url
  :defer t
  :commands (mb-url-http-around-advice)
  :init
  (setq mb-url-http-backend 'mb-url-http-curl
        mb-url-http-curl-switches `("--max-time" "20" "-x" ,"socks5h://127.0.0.1:1090"))
  (advice-add 'url-http :around 'mb-url-http-around-advice))

(provide 'init-eww)
;;; init-eww.el ends here
