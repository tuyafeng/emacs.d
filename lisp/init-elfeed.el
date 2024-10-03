;;; init-elfeed.el --- RSS Reader -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package elfeed
  :commands (elfeed)
  :config

  ;; Reference: https://emacs-china.org/t/elfeed-nerd-icons/26125
  (defun lucius/elfeed-search-print-entry--better-default (entry)
    "Print ENTRY to the buffer."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
           (date-width (car (cdr elfeed-search-date-format)))
           (title (concat (or (elfeed-meta entry :title)
                              (elfeed-entry-title entry)
                              "")
                          ;; NOTE: insert " " for overlay to swallow
                          " "))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title (when feed (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (mapconcat (lambda (s) (propertize s 'face 'elfeed-search-tag-face)) tags ","))
           (title-width (- (frame-width)
                           ;; (window-width (get-buffer-window (elfeed-search-buffer) t))
                           date-width elfeed-search-trailing-width))
           (title-column (elfeed-format-column
                          title (elfeed-clamp
                                 elfeed-search-title-min-width
                                 title-width
                                 elfeed-search-title-max-width) :left))


           ;; Title/Feed ALIGNMENT
           (align-to-feed-pixel (+ date-width
                                   (max elfeed-search-title-min-width
                                        (min title-width elfeed-search-title-max-width)))))
      (insert (propertize date 'face 'elfeed-search-date-face) " ")
      (insert (propertize title-column 'face title-faces 'kbd-help title))
      (put-text-property (1- (point)) (point) 'display `(space :align-to ,align-to-feed-pixel))
      ;; (when feed-title (insert " " (propertize feed-title 'face 'elfeed-search-feed-face) " "))
      (when feed-title
        (insert "  "
                (propertize feed-title 'face 'elfeed-search-feed-face) " "))
      (when tags (insert "(" tags-str ")"))))

  (setq elfeed-search-print-entry-function #'lucius/elfeed-search-print-entry--better-default)

  (defun my/elfeed-open-with-eww ()
    "Open current elfeed entry in eww."
    (interactive)
    (when-let ((entry (or elfeed-show-entry
                          (elfeed-search-selected :single))))
      (elfeed-untag entry 'unread)
      (eww (elfeed-entry-link entry))
      (elfeed-db-save-safe)))
  (define-key elfeed-show-mode-map (kbd "e") #'my/elfeed-open-with-eww)
  (define-key elfeed-search-mode-map (kbd "e") #'my/elfeed-open-with-eww))

(use-package elfeed-protocol
  :ensure t
  :demand t
  :after elfeed
  :custom
  (elfeed-use-curl t)
  (elfeed-set-timeout 36000)
  (elfeed-log-level 'debug)
  (elfeed-protocol-fever-update-unread-only t)
  (elfeed-curl-extra-arguments '("--insecure"))
  :config
  (defun my/reload-elfeed-protocol-feeds ()
    "Reload elfeed protocol feeds."
    (interactive)
    (when-let* ((entry "apps/freshrss")
                (fever (password-store-get-field entry "fever"))
                (url (password-store-get-field entry "url")))
      (setq elfeed-protocol-feeds
            (list
             (list fever
                   :api-url url
                   :password (list 'password-store-get entry))))))
  (elfeed-protocol-enable)
  (my/reload-elfeed-protocol-feeds))

(provide 'init-elfeed)
;;; init-elfeed.el ends here
