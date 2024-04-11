;;; init-android.el --- Configuration of packages for Android development -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package android-mode
  :defer t)

(defun my/phone-screenshot ()
  "Take screenshot for android phone using adb."
  (interactive)
  (let* (
         (temp-dir (file-name-concat temporary-file-directory "screenshots"))
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
  "Run scrcpy for connected android device."
  (interactive)
  (async-shell-command "scrcpy --hid-keyboard  --always-on-top  --window-width 340 --shortcut-mod=rctrl+rsuper"))

(provide 'init-android)
;;; init-android.el ends here
