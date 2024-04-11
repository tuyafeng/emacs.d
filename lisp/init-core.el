;;; init-core.el --- Common configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Reference: https://github.com/doomemacs/doomemacs/blob/develop/core/core.el

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq load-prefer-newer t)

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Disabled RTL support to optimize the display of extremely long text lines.
;; Reference: https://emacs-china.org/t/topic/25811/9
(defun my/find-file-disable-rtl-hook ()
  "Disabled RTL support when editing files."
  (setq bidi-display-reordering nil
        bidi-paragraph-direction nil))
(add-hook 'find-file-hook #'my/find-file-disable-rtl-hook)

(setq-default cursor-in-non-selected-windows nil)

(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)

(setq ffap-machine-p-known 'reject)

(setq frame-inhibit-implied-resize t)

(setq idle-update-delay 1.0)

(setq read-process-output-max (* 64 1024))

(setq redisplay-skip-fontification-on-input t)

(when (boundp 'w32-get-true-file-attributes)
  (setq w32-get-true-file-attributes nil
        w32-pipe-read-delay 0
        w32-pipe-buffer-size (* 64 1024))
  (setq selection-coding-system 'utf-8))

(unless (eq system-type 'darwin)
  (setq command-line-ns-option-alist nil))
(unless (eq system-type 'gnu/linux)
  (setq command-line-x-option-alist nil))

(setq ad-redefinition-action 'accept)

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(setq ring-bell-function #'ignore)

(fset 'display-startup-echo-area-message #'ignore)
(setq use-short-answers t)

(global-set-key (kbd "<escape>") 'keyboard-quit)

;; Load PATH from ~/.path, get your path by executing:
;; $ echo $PATH > ~/.path
(unless (eq system-type 'windows-nt)
  (condition-case err
    (let
        ((path (with-temp-buffer
                 (insert-file-contents-literally "~/.path")
                 (buffer-string))))
      (setenv "PATH" path)
      (setq exec-path (append (parse-colon-path path) (list exec-directory))))
  (error (warn "%s" (error-message-string err)))))

(provide 'init-core)
;;; init-core.el ends here
