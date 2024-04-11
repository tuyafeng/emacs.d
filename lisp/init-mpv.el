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
  (defun my/mpv-play-music ()
    "Play music with mpv."
    (interactive)
    (let* ((is-m3u-file (and (buffer-file-name)
                             (string= (file-name-extension buffer-file-name) "m3u")))
           (playlist-file (if is-m3u-file (buffer-file-name)
                            (expand-file-name "~/Music/Local2/playlist.m3u")))
           (args (list "--volume=30" "--shuffle" "--no-video"
                       "--loop-playlist=inf" "--no-resume-playback"
                       playlist-file)))
      (when is-m3u-file
        (add-to-list 'args (format "--playlist-start=%d"
                                   (- (line-number-at-pos) 1))))
      (if (file-exists-p playlist-file)
          (apply 'mpv-start args)
        (message "%s does not exist" playlist-file))))
  (setq mpv-volume-step 1.1))

(provide 'init-mpv)
;;; init-mpv.el ends here
