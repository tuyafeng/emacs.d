;;; init-mpv.el --- mpv.el configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package mpv
  :commands (mpv-play my/mpv-play-music)
  :init
  (defun my/mpv-find-file-hook()
    (when (string= (file-name-extension buffer-file-name) "m3u")
      (local-set-key (kbd "C-<return>") 'my/mpv-play-from-line)
      (read-only-mode 1)))
  (add-hook 'find-file-hook #'my/mpv-find-file-hook)
  :config
  ;; Reference: https://www.reddit.com/r/emacs/comments/y6jng9/mpvstart_failed_to_connect_to_mpv/
  ;; to address Failed to connect to mpv error
  (setq mpv-start-timeout 5)
  (setq mpv-volume-step 1.1)

  (defun my/mpv-play-from-line ()
    "Play the current M3U playlist from the current line."
    (interactive)
    (unless (and buffer-file-name
                 (equal (file-name-extension buffer-file-name) "m3u"))
      (user-error "Not an M3U playlist buffer"))
    (mpv-start "--volume=30" "--shuffle" "--no-video"
               "--loop-playlist=inf" "--no-resume-playback"
               (format "--playlist-start=%d" (1- (line-number-at-pos)))
               buffer-file-name))

  (defun my/mpv-play-music ()
    "Select and play music from the M3U playlist."
    (interactive)
    (unless (equal (file-name-extension my/mpv-music-playlist) "m3u")
      (user-error "Invalid M3U playlist: %s" my/mpv-music-playlist))
    (with-temp-buffer
      (insert-file-contents my/mpv-music-playlist)
      (let* ((songs (split-string (buffer-string) "\n" t))
             (song (completing-read "Play music: " songs nil t)))
        (unless (string-empty-p song)
          (mpv-start "--volume=30" "--shuffle" "--no-video"
                     "--loop-playlist=inf" "--no-resume-playback"
                     (format "--playlist-start=%d"
                             (seq-position songs song #'equal))
                     (expand-file-name my/mpv-music-playlist)))))))

(defcustom my/mpv-music-playlist ""
  "Path to the music playlist."
  :type 'file
  :group 'multimedia)

(with-eval-after-load 'launcher
  (defun my/music-candidates ()
    "Read songs from `my/mpv-music-playlist'."
    (let ((playlist (expand-file-name my/mpv-music-playlist)))
      (unless (equal (file-name-extension playlist) "m3u")
        (user-error "Invalid M3U playlist: %s" playlist))
      (with-temp-buffer
        (insert-file-contents playlist)
        (let ((songs
               (seq-remove
                (lambda (line) (string-prefix-p "#" line))
                (split-string (buffer-string) "[\r\n]+" t))))
          (cl-loop for song in songs
                   for index from 0
                   collect
                   (list :id index
                         :title (file-name-nondirectory song)
                         :data (list :playlist playlist :index index)))))))

  (defun my/play-song (candidate)
    "Play the song selected in Launcher."
    (require 'mpv)
    (let ((data (plist-get candidate :data)))
      (mpv-start "--volume=30" "--shuffle" "--no-video"
                 "--loop-playlist=inf" "--no-resume-playback"
                 (format "--playlist-start=%d"
                         (plist-get data :index))
                 (plist-get data :playlist)))
    nil)

  (add-to-list 'launcher-tools
               `("play" :name "Play music"
                 :icon ,(when (require 'nerd-icons nil t)
                          (nerd-icons-faicon "nf-fa-music"))
                 :candidates my/music-candidates
                 :submit my/play-song)))

(provide 'init-mpv)
;;; init-mpv.el ends here
