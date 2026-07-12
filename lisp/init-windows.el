;;; init-windows.el --- Working with emacs windows within frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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

(use-package winner
  :hook (after-init . winner-mode)
  :config
  (global-set-key (kbd "s-[") 'winner-undo)
  (global-set-key (kbd "s-]") 'winner-redo))

(provide 'init-windows)
;;; init-windows.el ends here
