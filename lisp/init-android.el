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
            (my/copy-file-to-clipboard path)
          (dired-jump-other-window path))
      (message "Failed to take screenshot(code: %d)." result))))

(defun my/scrcpy ()
  "Run scrcpy for connected Android device in a dedicated buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*scrcpy*"))
        (command "scrcpy --keyboard=aoa --always-on-top --window-width 340 --shortcut-mod=rctrl+rsuper"))
    (with-current-buffer buffer
      (unless (get-buffer-process buffer)
        (async-shell-command command buffer))
      (local-set-key (kbd "q") 'kill-buffer-and-window)
      (display-buffer buffer))))

(provide 'init-android)
;;; init-android.el ends here
