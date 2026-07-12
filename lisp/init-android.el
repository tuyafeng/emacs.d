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

(defun my/save-clipboard-image-to-file (file)
  "Save macOS clipboard image to PNG FILE using pngpaste."
  (interactive "FSave clipboard image to file: ")
  (let ((file (expand-file-name file)))
    (make-directory (file-name-directory file) t)
    (if (= 0 (call-process "pngpaste" nil nil nil file))
        (message "Saved clipboard image to %s" file)
      (user-error "Clipboard does not contain an image"))))

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

(use-package kotlin-ts-mode
  :mode ("\\.kt\\'" . kotlin-ts-mode))

(use-package java-ts-mode
  :ensure nil
  :mode ("\\.java\\'" . java-ts-mode)
  :config
  (add-to-list 'major-mode-remap-alist
               '(java-mode . java-ts-mode)))


(provide 'init-android)
;;; init-android.el ends here
