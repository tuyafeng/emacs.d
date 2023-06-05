;;; init-gui-frames.el --- Behaviour specific to non-TTY frames -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(set-face-attribute 'default nil :family "Menlo" :height 160)
(setq-default line-spacing 0.3)

;; Moved to early-init.el
;; (push '(menu-bar-lines . 0) default-frame-alist)
;; (push '(tool-bar-lines . 0) default-frame-alist)
;; (push '(vertical-scroll-bars) default-frame-alist)

;; Center emacs frame, and set its size to 60% width and 80% height
(let* ((screen-width (nth 3 (assq 'geometry (frame-monitor-attributes))))
       (screen-height (nth 4 (assq 'geometry (frame-monitor-attributes))))
       (char-width (frame-char-width))
       (char-height (frame-char-height))
       (frame-width  (/ (* 3 screen-width) (* 5 char-width)))
       (frame-height (/ (* 4 screen-height) (* 5 char-height)))
       (frame-top (/ screen-height 10))
       (frame-left (/ screen-width 5))
       )
  (add-to-list 'default-frame-alist (cons 'top frame-top))
  (add-to-list 'default-frame-alist (cons 'left frame-left))
  (add-to-list 'default-frame-alist (cons 'height frame-height))
  (add-to-list 'default-frame-alist (cons 'width frame-width))
  (add-to-list 'initial-frame-alist (cons 'height frame-height))
  (add-to-list 'initial-frame-alist (cons 'width frame-width))
  )

(setq use-file-dialog nil)
(setq use-dialog-box nil)

(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
