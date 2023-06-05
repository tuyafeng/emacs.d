;;; init-utils.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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

(defun my/delete-current-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun my/rename-current-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "New name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun my/browse-current-file ()
  "Open the current file using `browse-url`."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(provide 'init-utils)
;;; init-utils.el ends here
