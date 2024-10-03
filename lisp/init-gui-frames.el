;;; init-gui-frames.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun set-font-if-available (font-name cn-font-name &optional initial-size cn-font-rescale-ratio)
  "Set different fonts for Latin and Chinese characters, if the fonts are available."
  (let* ((size (or initial-size 14))
         (ratio (or cn-font-rescale-ratio 1.0))
         (main-font (font-spec :name font-name :size size))
         (cn-font (font-spec :name cn-font-name)))
    (if (and (find-font main-font) (find-font cn-font))
        (progn
          (set-face-attribute 'default nil :font main-font)
          (dolist (charset '(kana han symbol cjk-misc bopomofo))
            (set-fontset-font t charset cn-font))
          (setq face-font-rescale-alist `((,cn-font-name . ,ratio))))
      (message "One or both fonts do not exist: %s, %s" font-name cn-font-name))))

(if (eq system-type 'darwin)
    (set-font-if-available "IBM Plex Mono" "LXGW WenKai" 16 1.1)
  (set-font-if-available "IBM Plex Mono" "LXGW WenKai" 18 1.1))

(setq-default line-spacing 0.3)

(setq use-file-dialog nil)
(setq use-dialog-box nil)

(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
