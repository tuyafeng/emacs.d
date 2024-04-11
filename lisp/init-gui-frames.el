;;; init-gui-frames.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(let ((font-size (if (eq system-type 'windows-nt) 130 160))
      (default-font-faimly "Menlo")
      (cjk-font-faimly "PingFang SC"))
  (set-face-attribute 'default nil
                      :family default-font-faimly
                      :height font-size)
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family cjk-font-faimly :height font-size))))

(setq-default line-spacing 0.3)

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
