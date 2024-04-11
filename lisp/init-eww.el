;;; init-eww.el --- EWW configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eww
  :ensure nil
  :commands (my/eww-visit-bookmark eww)
  :config
  (setq shr-max-width 100
        eww-search-prefix "https://html.duckduckgo.com/html?q=")

  (defun my/eww-open-url-at-point-with-external-browser ()
    "Open the URL at point with an external browser."
    (interactive)
    (let ((links (eww-links-at-point)))
      (when links
        (let ((url (completing-read "Select URL: " links)))
          (eww-browse-with-external-browser url)))))
  (define-key eww-link-keymap (kbd "o")
              #'my/eww-open-url-at-point-with-external-browser)

  ;; Reference: https://emacs.stackexchange.com/a/38639
  (defun my/eww-toggle-images ()
    "Toggle whether images are loaded and reload the current page from cache."
    (interactive)
    (setq-local shr-inhibit-images (not shr-inhibit-images))
    (if (derived-mode-p 'eww-mode)
        (eww-reload t))
    (message "Images are now %s"
             (if shr-inhibit-images "off" "on")))
  (define-key eww-mode-map (kbd "I") #'my/eww-toggle-images)
  (define-key eww-link-keymap (kbd "I") #'my/eww-toggle-images)

  (defun my/eww-toggle-fonts ()
    "Toggle whether fonts are loaded and reload the current page from cache."
    (interactive)
    (setq-local shr-use-fonts (not shr-use-fonts))
    (if (derived-mode-p 'eww-mode)
        (eww-reload t))
    (message "Fonts are now %s"
             (if shr-use-fonts "on" "off")))
  (define-key eww-mode-map (kbd "F") #'my/eww-toggle-fonts)
  (define-key eww-link-keymap (kbd "F") #'my/eww-toggle-fonts)

  ;; Minimal rendering by default
  (setq-default shr-inhibit-images t)   ; toggle with `I`
  (setq-default shr-use-fonts nil)      ; toggle with `F`

  (define-key eww-mode-map (kbd "[") #'eww-back-url)
  (define-key eww-mode-map (kbd "]") #'eww-forward-url)

  (defun my/eww-rename-buffer ()
    (when (eq major-mode 'eww-mode)
      (when-let ((string (or (plist-get eww-data :title)
                             (plist-get eww-data :url)))
                 (max-length 58))
        (if (and (> max-length 3) (> (length string) max-length))
            (format "*%s...*" (substring string 0 (- max-length 3)))
          (format "*%s*" string)))))
  (setq eww-auto-rename-buffer #'my/eww-rename-buffer)

  (defun my/eww-add-bookmark ()
    "Bookmark the current page with the given title."
    (interactive)
    (when-let ((url (plist-get eww-data :url))
               (title (read-string "Set bookmark title: "
                                   (plist-get eww-data :title))))
      (setq title (replace-regexp-in-string "[\n\t\r]" "" title))
      (setq title (replace-regexp-in-string "\\` +\\| +\\'" "" title))
      ;; Delete existing bookmarks with the same URL
      (unless eww-bookmarks
        (eww-read-bookmarks))
      (setq eww-bookmarks (cl-remove-if (lambda (bookmark)
                                          (equal url (plist-get bookmark :url)))
                                        eww-bookmarks))
      (push (list :url url
		          :title title
		          :time (current-time-string))
	        eww-bookmarks)
      (eww-write-bookmarks)
      (message "Bookmarked %s (%s)" url title)))
  (define-key eww-mode-map (kbd "b") #'my/eww-add-bookmark)

  (defun my/eww-visit-bookmark ()
    "Visit a bookmarked URL."
    (interactive)
    (unless eww-bookmarks
      (eww-read-bookmarks))
    (let ((urls (mapcar (lambda (bookmark)
                          (plist-get bookmark :url))
                        eww-bookmarks)))
      (let ((url (completing-read "Enter URL or keywords: " urls)))
        (eww url))))
  (define-key eww-mode-map (kbd "B") #'my/eww-visit-bookmark)

  (define-key eww-mode-map (kbd "L") #'eww-list-bookmarks))

(use-package eww-url
  :after eww
  :load-path "site-lisp/eww-url")

(use-package mb-url
  :config
  (setq mb-url-http-backend 'mb-url-http-curl)
  (setq mb-url-http-curl-default-switches
        '("--max-time" "5" "--user-agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
        mb-url-http-curl-switches mb-url-http-curl-default-switches)
  (defun my/mb-url-toggle-proxy ()
    "Toggle whether proxy is enabled."
    (interactive)
    (let ((proxy "socks5h://127.0.0.1:1090")
          (switches mb-url-http-curl-default-switches))
      (unless (and mb-url-http-curl-switches
                   (member "-x" mb-url-http-curl-switches))
        (push proxy switches)
        (push "-x" switches))
      (setq mb-url-http-curl-switches switches)
      (message "Proxy is now %s"
               (if (member "-x" switches) "enabled" "disabled"))))
  (with-eval-after-load 'eww
    (define-key eww-mode-map "P" 'my/mb-url-toggle-proxy))
  (defun my/mb-url-emacs-startup-hook ()
    (advice-add 'url-http :around 'mb-url-http-around-advice))
  (add-hook 'emacs-startup-hook #'my/mb-url-emacs-startup-hook))

(provide 'init-eww)
;;; init-eww.el ends here
