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
        tab-bar-tab-name-truncated-max 20
        tab-bar-auto-width nil)

  (defun my--tab-bar-space ()
    "Return a fixed width space for the tab-bar."
    " ")
  (setq tab-bar-format '(tab-bar-format-tabs my--tab-bar-space))

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

(use-package tabspaces
  :hook (after-init . tabspaces-mode)
  :bind
  (:map tabspaces-command-map
        ("n" . tab-new)
        ("k" . my/tabspaces-kill-buffers-close-workspace))
  :config

  (defun my/tabspaces-kill-buffers-close-workspace ()
    "Kill all buffers in the current workspace, then close its tab.
After closing, show the previous tab if possible, otherwise stay at first tab."
    (interactive)
    (let* ((current (1+ (tab-bar--current-tab-index))) ;; 1-based
           (target  (if (> current 1) (1- current) 1))
           (bufs    (tabspaces--buffer-list)))
      (unwind-protect
          (mapc #'kill-buffer bufs)
        (tab-bar-close-tab current target))))

  (defvar my/tabspaces-repeat-map
    (let ((map (make-sparse-keymap)))
      (dolist (pair '(("n" . tab-new)
                      ("k" . my/tabspaces-kill-buffers-close-workspace)))
        (let ((key (car pair))
              (fn  (cdr pair)))
          (keymap-global-set (concat "C-c w " key) fn)
          (define-key map (kbd key) fn)
          (put fn 'repeat-map 'my/tabspaces-repeat-map)))
      map)
    "Tabspaces repeat map for `C-c w ...` commands.")

  ;; Filter Buffers for Consult-Buffer
  (with-eval-after-load 'consult
    ;; hide full buffer list (still available with "b" prefix)
    (consult-customize consult--source-buffer :hidden t :default nil)
    ;; set consult-workspace buffer list
    (defvar consult--source-workspace
      (list :name     "Workspace Buffers"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                                  :predicate #'tabspaces--local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))

      "Set workspace buffer list for consult-buffer.")
    (add-to-list 'consult-buffer-sources 'consult--source-workspace))

  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-keymap-prefix "C-c w")
  (tabspaces-initialize-project-with-todo nil))

(provide 'init-tab-bar)
;;; init-tab-bar.el ends here
