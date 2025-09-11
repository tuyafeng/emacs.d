;;; init-tab-bar.el --- Tab bar configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package tab-bar
  :ensure nil
  :hook (window-setup . tab-bar-mode)
  :config
  (setq tab-bar-new-tab-to 'rightmost
        tab-bar-separator ""
        tab-bar-show 1
        tab-bar-close-button-show nil
        tab-bar-new-tab-choice "*scratch*"
        ;; Show tab numbers
        tab-bar-tab-hints t
        tab-bar-format '(tab-bar-format-tabs tab-bar-separator)
        tab-bar-tab-name-truncated-max 20
        tab-bar-auto-width nil)
  ;; Reference: https://emacs-china.org/t/tab-bar/26008
  (defun my--tab-bar-tab-name ()
    "Generate tab name based on current buffer, truncate if too long,
and append window count if more than one."
    (let* ((raw-tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
           (count (length (window-list-1 nil 'nomini)))
           (truncated-tab-name
            (if (< (length raw-tab-name) tab-bar-tab-name-truncated-max)
                raw-tab-name
              (truncate-string-to-width raw-tab-name
                                        tab-bar-tab-name-truncated-max
                                        nil nil tab-bar-tab-name-ellipsis))))
      (if (> count 1)
          (concat truncated-tab-name "(" (number-to-string count) ")")
        truncated-tab-name)))
  (setq tab-bar-tab-name-function #'my--tab-bar-tab-name)
  (defun my--tab-bar-tab-name-format (tab i)
    "Format TAB name with index I, applying faces for active/inactive states.
Adds a bold underlined index before the tab name."
    (let ((face (funcall tab-bar-tab-face-function tab)))
      (concat
       (propertize " " 'face face)
       (propertize (number-to-string i)
                   'face `(:inherit ,face :weight ultra-bold :underline t))
       (propertize (concat " " (alist-get 'name tab) " ") 'face face))))
  (setq tab-bar-tab-name-format-function #'my--tab-bar-tab-name-format)
  :custom
  (tab-bar-select-tab-modifiers '(super)))

(provide 'init-tab-bar)
;;; init-tab-bar.el ends here
