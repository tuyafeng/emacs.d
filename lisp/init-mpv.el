;;; init-mpv.el --- mpv.el configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package mpv
  :commands (mpv-play my/mpv-play-music)
  :init
  (defun my/mpv-find-file-hook()
    (when (string= (file-name-extension buffer-file-name) "m3u")
      (local-set-key (kbd "C-<return>") 'my/mpv-play-music)
      (read-only-mode 1)))
  (add-hook 'find-file-hook #'my/mpv-find-file-hook)
  :config
  ;; Reference: https://www.reddit.com/r/emacs/comments/y6jng9/mpvstart_failed_to_connect_to_mpv/
  ;; to address Failed to connect to mpv error
  (setq mpv-start-timeout 5)
  (defun my/mpv-play-music ()
  "Play the current .m3u file with mpv from the current line."
  (interactive)
  (if (and buffer-file-name (string= (file-name-extension buffer-file-name) "m3u"))
      (apply #'mpv-start
             (list "--volume=30" "--shuffle" "--no-video"
                   "--loop-playlist=inf" "--no-resume-playback"
                   (format "--playlist-start=%d" (1- (line-number-at-pos)))
                   buffer-file-name))
    (user-error "Not an .m3u playlist buffer")))
  (setq mpv-volume-step 1.1))

(provide 'init-mpv)
;;; init-mpv.el ends here
