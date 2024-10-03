;;; early-init.el --- Emacs 27+ pre-initialisation config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Reference: https://www.reddit.com/r/emacs/comments/ofhket/further_boost_start_up_time_with_a_simple_tweak/
(let ((normal-gc-cons-threshold (* 32 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024))
      (default-file-name-handler-alist file-name-handler-alist))
  (setq gc-cons-threshold init-gc-cons-threshold
        file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold
                             file-name-handler-alist default-file-name-handler-alist
                             ))))

(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (internal-border-width . 0)
        (horizontal-scroll-bars)
        (vertical-scroll-bars)))

(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
