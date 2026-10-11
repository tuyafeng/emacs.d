;;; init-launcher.el --- Launcher -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package launcher
  :vc (:url "https://github.com/xiaoxinghu/launcher.el")
  :bind ("C-c l" . launcher)
  :config
  (when (featurep 'no-littering)
    (setq launcher-icon-cache-directory
          (no-littering-expand-var-file-name "launcher/")))

  (defun my/launcher-collect-apps (paths)
    "Include applications missing from PATHS."
    (dolist (dir launcher-app-directories)
      (when (file-directory-p dir)
        (setq paths
              (nconc paths
                     (directory-files dir t "\\.app\\'")))))
    (delete-dups paths))

  (with-eval-after-load 'launcher
    (advice-add #'launcher--collect-paths
                :filter-return #'my/launcher-collect-apps))

  (setq launcher-bangs
        '(("g" "Google" "https://www.google.com.hk/search?newwindow=1&q=%s")
          ("bd" "Baidu" "https://www.baidu.com/s?ie=UTF-8&wd=%s")
          ("ddg" "DuckDuckGo" "https://duckduckgo.com/?ia=web&q=%s")
          ("bl" "Bilibili" "https://search.bilibili.com/all?keyword=%s")
          ("yt" "YouTube" "https://www.youtube.com/results?search_query=%s")
          ("gh" "GitHub" "https://github.com/search?type=repositories&s=stars&o=desc&q=%s")
          ("ec" "Emacs China" "https://www.google.com.hk/search?newwindow=1&q=%s+site:emacs-china.org")
          ("vx" "V2EX" "https://www.google.com.hk/search?newwindow=1&q=%s+site:v2ex.com")))

  (setq launcher-fallback-search-url "https://www.google.com.hk/search?newwindow=1&q=%s")
  (setq browse-url-browser-function #'browse-url-default-browser)

  (defun my/launcher-align (items)
    "Align Launcher annotations at column 24."
    (mapcar (lambda (item)
              (when-let* ((ann (nth 2 item))
                          ((not (string-empty-p ann))))
                (setf (nth 2 item)
                      (concat
                       (propertize " " 'display
                                   '(space :align-to 24))
                       (propertize (string-trim-left ann)
                                   'face 'marginalia-documentation))))
              item)
            items))
  (advice-add #'launcher--affixation :filter-return #'my/launcher-align))

(defun my/calc-live ()
  "Calculate with live completion and copy the result."
  (interactive)
  (require 'calc)
  (let* ((completion-styles '(basic))
         (input
          (completing-read
           "Calculate: "
           (completion-table-dynamic
            (lambda (input)
              (when (string-match-p "\\`[0-9.+*/() -]+\\'" input)
                (condition-case nil
                    (list (concat input " = " (calc-eval input)))
                  (error nil)))))))
         (expr (replace-regexp-in-string " = .*\\'" "" input))
         (result (calc-eval expr)))
    (kill-new result)
    (message "Copied: %s" result)))

(global-set-key (kbd "C-c c") #'my/calc-live)

(defun my/launcher ()
  "Open launcher."
  (interactive)
  (condition-case nil
      (if-let* ((win (active-minibuffer-window)))
          (select-frame-set-input-focus (window-frame win))
        (select-frame-set-input-focus (selected-frame))
        (call-interactively #'launcher))
    (quit nil)))

(defun my/empty-trash ()
  "Empty the macOS Trash."
  (interactive)
  (when (yes-or-no-p "Empty Trash permanently? ")
    (start-process
     "empty-trash" nil
     "osascript" "-e"
     "tell application \"Finder\" to empty the trash")))

(defun my/eject-volume ()
  "Select and eject a mounted volume."
  (interactive)
  (let* ((volumes (seq-filter
                   (lambda (path)
                     (and (file-directory-p path)
                          (not (file-symlink-p path))))
                   (directory-files "/Volumes" t directory-files-no-dot-files-regexp)))
         (volume (if volumes
                     (completing-read "Eject volume: " volumes nil t)
                   (user-error "No mounted volumes found")))
         (script (format
                  "tell application \"Finder\" to eject POSIX file %s as alias"
                  (prin1-to-string volume))))
    (let ((buffer (get-buffer-create "*eject-volume*")))
      (with-current-buffer buffer
        (erase-buffer))
      (set-process-sentinel
       (start-process "eject-volume" buffer "osascript" "-e" script)
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (if (and (eq (process-status proc) 'exit)
                    (zerop (process-exit-status proc)))
               (message "Volume ejected")
             (display-buffer (process-buffer proc)))))))))

(provide 'init-launcher)
;;; init-launcher.el ends here
