;;; init-windows.el --- Working with emacs windows within frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :config
  (setq winner-dont-bind-my-keys t)
  (setq winner-repeat-map
        '(keymap (?u . winner-redo) (?r . winner-undo)))
  (global-set-key (kbd "C-c u") 'winner-undo)
  (global-set-key (kbd "C-c r") 'winner-redo))

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
