;;; init-utils.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Reference: https://github.com/gmoutso/dotemacs/blob/93649716da46497dd79d07e06a30e694b9207a2b/lisp/variousrc.el
(defun my/copy-file-to-clipboard (&optional file)
  "Copy file at point to clipboard."
  (interactive)
  (let ((file (or file (if (derived-mode-p 'dired-mode)
                           (dired-get-file-for-visit)
                         (buffer-file-name)))))
    (when (and file (file-regular-p file))
      (cond
       ((eq system-type 'windows-nt)
        (message "Not supported yet."))
       ((eq system-type 'darwin)
        (do-applescript
         (format "set the clipboard to POSIX file \"%s\"" (expand-file-name file))))
       ((eq system-type 'gnu/linux)
        (call-process-shell-command
         (format "xclip -selection clipboard -t %s -i %s"
                 (mailcap-extension-to-mime (file-name-extension file))
                 file))))
      (message "Copied %s" file))))

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

(provide 'init-utils)
;;; init-utils.el ends here
