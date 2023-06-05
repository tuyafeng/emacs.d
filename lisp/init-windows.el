;;; init-windows.el --- Working with emacs windows within frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Enable winner mode to quickly restore window configurations
(add-hook 'after-init-hook 'winner-mode)
(global-set-key (kbd "M-s-[") 'winner-undo)
(global-set-key (kbd "M-s-]") 'winner-redo)

(use-package switch-window
  :commands (switch-window)
  :bind
  ("M-o" . 'switch-window)
  :config
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-timeout nil)
  (setq switch-window-minibuffer-shortcut ?z))

;; When splitting window, show (other-buffer) in the new window
;; Reference: https://github.com/purcell/emacs.d/blob/master/lisp/init-windows.el

(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

;; Rearrange split windows
;; Reference: https://github.com/purcell/emacs.d/blob/master/lisp/init-windows.el

(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(global-set-key (kbd "C-x |") 'split-window-horizontally-instead)
(global-set-key (kbd "C-x _") 'split-window-vertically-instead)

(provide 'init-windows)
;;; init-windows.el ends here
