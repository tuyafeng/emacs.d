;;; init-gui-frames.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(let ((font-size (if (eq system-type 'windows-nt) 130 160)))
  (set-face-attribute 'default nil :family "Menlo" :height font-size))
(setq-default line-spacing 0.3)

;; Moved to early-init.el
;; (push '(menu-bar-lines . 0) default-frame-alist)
;; (push '(tool-bar-lines . 0) default-frame-alist)
;; (push '(vertical-scroll-bars) default-frame-alist)

(setq use-file-dialog nil)
(setq use-dialog-box nil)

(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
