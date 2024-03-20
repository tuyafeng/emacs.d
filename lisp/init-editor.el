;;; init-editor.el --- Editor configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(blink-cursor-mode -1)
(global-display-line-numbers-mode 1)
(setq-default display-line-numbers-width 3)

(use-package beacon
  :if (display-graphic-p)
  :diminish beacon-mode
  :hook (after-init . beacon-mode))

(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq auto-revert-verbose nil
        ;; Revert Dired buffers too
        global-auto-revert-non-file-buffers t)
  (when (eq system-type 'darwin)
    ;; File notifications aren't supported on OS X
    (setq auto-revert-use-notify nil)))

(use-package files
  :ensure nil
  :config
  (setq create-lockfiles nil)
  (setq make-backup-files nil)
  ;; In case the user does enable backup
  (setq version-control t)
  (setq backup-by-copying t)
  (setq delete-old-versions t)
  (setq make-backup-files nil)
  (setq kept-old-versions 5)
  (setq kept-new-versions 5)
  ;; Turn on auto-save as fallback strategy in case of crashes or lost data
  (setq auto-save-default t)
  (setq auto-save-visited-mode t)
  (setq auto-save-include-big-deletions t)
  ;; set file path for backup and auto-saved files
  ;; (setq backup-directory-alist `((".*" . "~/.emacs.d/auto-save")))
  ;; (setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-save" t)))
  )

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package simple
  :ensure nil
  :diminish visual-line-mode
  :hook (after-init . global-visual-line-mode))

(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-l") 'my/select-current-line)

(defun my/select-current-line (arg)
  "Select the current line and move the cursor by ARG lines IF
    no region is selected.

    If a region is already selected when calling this command, only move
    the cursor by ARG lines."
  (interactive "p")
  (when (not (use-region-p))
    (forward-line 0)
    (set-mark-command nil))
  (forward-line arg))

(use-package undo-fu
  :bind
  ("s-z" . 'undo-fu-only-undo)
  ("s-Z" . 'undo-fu-only-redo))

(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :bind (:map prog-mode-map
              ("C-{" . hs-hide-block)
              ("C-}" . hs-show-block))
  :hook (prog-mode . hs-minor-mode))

(use-package so-long
  :when (>= emacs-major-version 27)
  :ensure nil
  :hook (after-init . global-so-long-mode))

(use-package subword
  :ensure nil
  :diminish subword-mode
  :hook (after-init . global-subword-mode))

(setq-default indent-tabs-mode nil
              tab-width 4)
;; Make tab complete if the line is indented
(setq tab-always-indent 'complete)

(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode))

(use-package newcomment
  :ensure nil
  :bind
  (("s-/" . 'my/comment-or-uncomment))
  :config
  (defun my/comment-or-uncomment ()
    (interactive)
    (if (region-active-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
      (if (save-excursion
            (beginning-of-line)
            (looking-at "\\s-*$"))
          (call-interactively 'comment-dwim)
        (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))
  :custom
  (comment-auto-fill-only-comments t))

;; Reference: http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)

(provide 'init-editor)
;;; init-editor.el ends here
