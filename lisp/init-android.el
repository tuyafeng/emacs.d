;;; init-android.el --- Configuration of packages for Android development -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun my/phone-screenshot ()
  "Take screenshot for Android phone using adb."
  (interactive)
  (let* ((temp-dir (file-name-concat temporary-file-directory "screenshots"))
         (filename (format-time-string "screenshot-%Y%m%d-%H%M%S.png"))
         (path (file-name-concat temp-dir filename))
         (command (format "mkdir -p %s && adb exec-out screencap -p > %s" temp-dir  path))
         (result (call-process-shell-command command)))
    (if (= 0 result)
        (if (y-or-n-p "Screenshot taken. Copy to clipboard? ")
            (my/copy-image-file-to-clipboard path)
          (dired-jump-other-window path))
      (message "Failed to take screenshot(code: %d)." result))))

(defun my/copy-image-file-to-clipboard (file)
  "Copy image FILE to macOS system clipboard."
  (interactive "fImage file: ")
  (let ((file (expand-file-name file)))
    (if (and (file-exists-p file)
             (= 0
                (call-process
                 "osascript" nil nil nil
                 "-e"
                 (format
                  "set the clipboard to (read (POSIX file \"%s\") as «class PNGf»)"
                  file))))
        (message "Copied %s to clipboard" file)
      (user-error "Failed to copy image file to clipboard: %s" file))))

(defun my/save-clipboard-image-to-file (dir)
  "Save macOS clipboard image to DIR using pngpaste."
  (interactive "DSave clipboard image to directory: ")
  (let* ((dir (expand-file-name dir))
         (filename (format-time-string "clipboard-%Y%m%d-%H%M%S.png"))
         (file (expand-file-name filename dir)))
    (make-directory dir t)
    (if (= 0 (call-process "pngpaste" nil nil nil file))
        (message "Saved clipboard image to %s" file)
      (user-error "Clipboard does not contain an image"))))

(defun my/scrcpy ()
  "Run scrcpy for connected Android device in a dedicated buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*scrcpy*"))
        (command "scrcpy --keyboard=uhid  --always-on-top  --window-width 300 --shortcut-mod=lctrl"))
    (with-current-buffer buffer
      (unless (get-buffer-process buffer)
        (async-shell-command command buffer))
      (local-set-key (kbd "q") 'kill-buffer-and-window)
      (display-buffer buffer))))

(defun my/scrcpy-record-stop ()
  "Stop recording the Android screen with scrcpy."
  (interactive)
  (when-let* ((proc (get-buffer-process "*scrcpy-record*")))
    (set-process-sentinel
     proc
     (lambda (p _)
       (when (memq (process-status p) '(exit signal))
         (let ((file (process-get p 'record-file))
               (buffer (process-buffer p)))
           (when (buffer-live-p buffer)
             (quit-windows-on buffer)
             (kill-buffer buffer))
           (message "%s: %s"
                    (if (and file (file-exists-p file))
                        "Recording saved"
                      "Recording failed")
                    file)
           ))))
    (interrupt-process proc)))

(defun my/scrcpy-record-start ()
  "Start recording the Android screen with scrcpy."
  (interactive)
  (let* ((buffer (get-buffer-create "*scrcpy-record*"))
         (file (expand-file-name
                (format-time-string "screenrecord-%Y%m%d-%H%M%S.mp4")
                "~/Downloads/")))
    (unless (get-buffer-process buffer)
      (async-shell-command
       (format "exec scrcpy --no-playback --no-audio --record=%s"
               (shell-quote-argument file))
       buffer)
      (process-put (get-buffer-process buffer) 'record-file file)
      (with-current-buffer buffer
        (use-local-map (copy-keymap (current-local-map)))
        (dolist (key '("q" "C-g" "<escape>"))
          (keymap-local-set key #'my/scrcpy-record-stop))))
    (pop-to-buffer buffer)))

(provide 'init-android)
;;; init-android.el ends here
