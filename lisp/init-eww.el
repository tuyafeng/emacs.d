;;; init-eww.el --- EWW configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eww
  :ensure nil
  :commands (my/eww-visit-bookmark eww)
  :init
  (setq browse-url-browser-function #'eww-browse-url)
  :config
  (setq shr-max-width 100)
  (setq eww-search-prefix "https://html.duckduckgo.com/html?q=")

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
  (define-key eww-mode-map (kbd "<mouse-4>") #'eww-back-url)
  (define-key eww-mode-map (kbd "<mouse-3>") #'eww-forward-url)

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

  (define-key eww-mode-map (kbd "L") #'eww-list-bookmarks)

  (defun my/eww--make-button (icon text fn &optional hint)
    "Return a header-line button.
ICON is a Nerd Font name string (e.g. \"nf-fa-arrow_left\").
TEXT is fallback text if `nerd-icons` is unavailable.
FN is the command to call.
HINT is optional mouse tooltip."
    (let ((label (if (featurep 'nerd-icons)
                     (nerd-icons-faicon icon)
                   text)))
      (propertize (concat " " label " ")
                  'mouse-face 'mode-line-highlight
                  'face '(:height 0.8)
                  'help-echo (or hint (symbol-name fn))
                  'keymap (let ((map (make-sparse-keymap)))
                            (define-key map [header-line mouse-1] fn)
                            map))))

  (defun my/eww-set-header-line ()
    "Set EWW header line with buttons and page info."
    (setq header-line-format
          (list
           (my/eww--make-button "nf-fa-arrow_left"
                                "Back"
                                #'eww-back-url
                                "Go back")
           (my/eww--make-button "nf-fa-arrow_right"
                                "Forward"
                                #'eww-forward-url
                                "Go forward")
           (my/eww--make-button "nf-fa-refresh"
                                "Reload"
                                #'eww-reload
                                "Reload page")
           (my/eww--make-button "nf-fa-share"
                                "Browser"
                                #'eww-browse-with-external-browser
                                "External browser")
           " "
           '(:eval
             (let ((title (or (and (boundp 'eww-data) (plist-get eww-data :title)) "Untitled"))
                   (url   (or (and (boundp 'eww-data) (plist-get eww-data :url)) "about:blank")))
               (propertize
                (if (string-empty-p title) url (format "%s: %s" title url))
                'face 'header-line))))))

  (add-hook 'eww-after-render-hook #'my/eww-set-header-line)

  (defun my/run-eww-after-render-hook (&rest _)
    "Run `eww-after-render-hook`."
    (run-hooks 'eww-after-render-hook))

  ;; Run after render hook after back/forward
  (advice-add 'eww-back-url :after #'my/run-eww-after-render-hook)
  (advice-add 'eww-forward-url :after #'my/run-eww-after-render-hook)

  (defun my/eww-save-history-filter (orig-fn &rest args)
    "Skip saving blank/redirect pages into `eww-history'."
    (let ((content (string-trim (buffer-string)))
          (title   (or (plist-get eww-data :title) "")))
      (unless (or (string-empty-p content)
                  (string-match-p "\\`Redirecting" title))
        (apply orig-fn args))))

  (advice-add 'eww-save-history :around #'my/eww-save-history-filter))

(use-package mb-url
  :config
  (setq mb-url-http-backend 'mb-url-http-curl)
  (setq mb-url-http-curl-default-switches
        '("--max-time" "5" "--user-agent" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
        mb-url-http-curl-switches mb-url-http-curl-default-switches)
  (with-eval-after-load 'elfeed
    (setq elfeed-curl-extra-arguments mb-url-http-curl-switches))
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
      (with-eval-after-load 'elfeed
        (setq elfeed-curl-extra-arguments mb-url-http-curl-switches))
      (message "Proxy is now %s"
               (if (member "-x" switches) "enabled" "disabled"))))
  (with-eval-after-load 'eww
    (define-key eww-mode-map "P" 'my/mb-url-toggle-proxy))
  (defun my/mb-url-emacs-startup-hook ()
    (advice-add 'url-http :around 'mb-url-http-around-advice))
  (add-hook 'emacs-startup-hook #'my/mb-url-emacs-startup-hook)
  (defun my/hide-mb-url-buffers ()
    "Hide buffers starting with *mb-url- in the buffer list."
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (string-prefix-p "*mb-url-" (buffer-name))
          (rename-buffer (concat " " (buffer-name)) 'unique)))))
  (add-hook 'buffer-list-update-hook 'my/hide-mb-url-buffers))

(provide 'init-eww)
;;; init-eww.el ends here
