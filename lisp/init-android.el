;;; init-android.el --- Configuration of packages for Android development -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun my/phone-screenshot ()
  "Take screenshot for android phone using adb."
  (interactive)
  (let* ((temp-dir (file-name-concat temporary-file-directory "screenshots"))
         (filename (format-time-string "screenshot-%Y%m%d-%H%M%S.png"))
         (path (file-name-concat temp-dir filename))
         (result (call-process-shell-command (format "%s" (list "mkdir -p" temp-dir "&& adb exec-out screencap -p >" path)) nil nil)))
    (if (= 0 result)
        (if (y-or-n-p "Copy the screenshot to clipboard?")
            (progn
              (my/copy-file-to-clipboard path))
          (progn
            (dired-jump-other-window path)))
      (message "Failed to take screenshot(code: %d)." result))))

(defun my/scrcpy ()
  "Run scrcpy for connected Android device in a dedicated buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*scrcpy*")
    (unless (get-buffer-process (current-buffer))
      (async-shell-command "scrcpy --keyboard=aoa --always-on-top --window-width 340 --shortcut-mod=rctrl+rsuper"
                           (current-buffer)))
    (local-set-key (kbd "q") 'kill-buffer-and-window)
    (display-buffer (current-buffer))))

(provide 'init-android)
;;; init-android.el ends here
