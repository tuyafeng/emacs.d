;;; init-launcher.el --- Launcher -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package launcher
  ;; :vc (:url "https://github.com/xiaoxinghu/launcher.el")
  :load-path (lambda ()
               (expand-file-name "site-lisp/launcher"
                                 user-emacs-directory))
  :bind ("C-c l" . launcher)
  :config
  (when (featurep 'no-littering)
    (setq launcher-icon-cache-directory
          (no-littering-expand-var-file-name "launcher/")))

  (setq launcher-app-discovery-method 'filesystem)
  (when (featurep 'nerd-icons)
    (setq launcher-builtin-icons
          `((calculator . ,(nerd-icons-faicon "nf-fa-calculator"))
            (search     . ,(nerd-icons-faicon "nf-fa-search"))
            (url        . ,(nerd-icons-faicon "nf-fa-link")))))

  (setq launcher-tools
        (cl-remove "g" launcher-tools :key #'car :test #'equal))

  (dolist (entry
           '(("g" "Google"
              "https://www.google.com.hk/search?newwindow=1&q=%s"
              "nf-fa-google")
             ("bd" "Baidu"
              "https://www.baidu.com/s?ie=UTF-8&wd=%s"
              "nf-fa-paw")
             ("ddg" "DuckDuckGo"
              "https://duckduckgo.com/?ia=web&q=%s"
              nil)
             ("bl" "Bilibili"
              "https://search.bilibili.com/all?keyword=%s"
              "nf-fa-tv")
             ("yt" "YouTube"
              "https://www.youtube.com/results?search_query=%s"
              "nf-fa-youtube_play")
             ("gh" "GitHub"
              "https://github.com/search?type=repositories&s=stars&o=desc&q=%s"
              "nf-fa-github")
             ("ec" "Emacs China"
              "https://www.google.com.hk/search?newwindow=1&q=%s+site:emacs-china.org"
              nil)
             ("vx" "V2EX"
              "https://www.google.com.hk/search?newwindow=1&q=%s+site:v2ex.com"
              nil)))
    (let ((prefix (nth 0 entry))
          (name   (nth 1 entry))
          (url    (nth 2 entry))
          (icon   (nth 3 entry)))
      (add-to-list
       'launcher-tools
       (launcher-search-tool
        prefix :name name :url url
        :icon (when (and icon
                         (not (equal icon ""))
                         (fboundp 'nerd-icons-faicon))
                (nerd-icons-faicon icon)))
       t)))

  (setq launcher-fallback-search-url "https://www.google.com.hk/search?newwindow=1&q=%s")
  (setq browse-url-browser-function #'browse-url-default-browser)

  (defun my/launcher-align-annotation (ann)
    "Align Launcher annotations."
    (when (and ann (not (string-empty-p ann)))
      (concat
       (propertize " " 'display '(space :align-to 32))
       (propertize (string-trim-left ann)
                   'face 'marginalia-documentation))))

  (with-eval-after-load 'launcher-query
    (advice-add 'launcher--results-annotation
                :filter-return
                #'my/launcher-align-annotation)))

;; Entry point invoked by skhd hotkeys

(defun my/launcher ()
  "Focus the active minibuffer or open Launcher."
  (interactive)
  (run-at-time
   0 nil
   (lambda ()
     (condition-case nil
         (if-let* ((win (active-minibuffer-window)))
             (select-frame-set-input-focus (window-frame win))
           (select-frame-set-input-focus (selected-frame))
           (call-interactively #'launcher))
       (quit nil)))))

;; Dictionary

(use-package osx-dictionary
  :after launcher
  :config
  (require 'launcher-osx-dictionary)
  (add-to-list 'launcher-tools
               `("d" :name "Dictionary"
                 :icon ,(when (require 'nerd-icons nil t)
                          (nerd-icons-faicon "nf-fa-book"))
                 :query launcher-osx-dictionary-query
                 :debounce 0.3
                 :submit launcher-osx-dictionary-submit)))

;; Empty trash

(defun my/empty-trash ()
  "Empty the macOS Trash."
  (interactive)
  (when (yes-or-no-p "Empty Trash permanently? ")
    (start-process
     "empty-trash" nil
     "osascript" "-e"
     "tell application \"Finder\" to empty the trash")))

(with-eval-after-load 'launcher
  (add-to-list
   'launcher-tools
   `("empty-trash" :name "Empty Trash"
     :icon ,(when (fboundp 'nerd-icons-faicon)
              (nerd-icons-faicon "nf-fa-trash"))
     :candidates ((:id empty-trash
                   :title "Empty Trash"
                   :subtitle "Permanently delete trash; confirmation required"))
     :submit (lambda (_candidate)
               (my/empty-trash)
               nil))))

;; Eject

(defun my/eject-volume-candidates ()
  "Return mounted volumes as launcher candidates."
  (let ((volumes
         (seq-filter
          (lambda (path)
            (and (file-directory-p path)
                 (not (file-symlink-p path))))
          (directory-files
           "/Volumes" t directory-files-no-dot-files-regexp))))
    (unless volumes
      (user-error "No mounted volumes found"))
    (mapcar
     (lambda (path)
       (list :id path
             :title (file-name-nondirectory path)
             :subtitle path
             :data path))
     volumes)))

(defun my/eject-volume-submit (candidate)
  "Eject the volume represented by CANDIDATE."
  (let* ((volume (plist-get candidate :data))
         (script
          (format
           "tell application \"Finder\" to eject POSIX file %s as alias"
           (prin1-to-string volume)))
         (buffer (get-buffer-create "*eject-volume*")))
    (with-current-buffer buffer
      (erase-buffer))
    (make-process
     :name "eject-volume"
     :buffer buffer
     :command (list "osascript" "-e" script)
     :noquery t
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (if (and (eq (process-status proc) 'exit)
                  (zerop (process-exit-status proc)))
             (message "Volume ejected: %s" volume)
           (when (buffer-live-p (process-buffer proc))
             (display-buffer (process-buffer proc)))))))
    nil))

(with-eval-after-load 'launcher
  (add-to-list
   'launcher-tools
   `("eject"
     :name "Eject Volume"
     :icon ,(when (fboundp 'nerd-icons-faicon)
             (nerd-icons-faicon "nf-fa-hdd_o"))
     :candidates my/eject-volume-candidates
     :submit my/eject-volume-submit)))

;; Quit tool

(defun my/quit-app-candidates ()
  "Return running foreground applications with launcher icons."
  (let* ((script
          "tell application \"System Events\" to get name of application processes whose background only is false")
         (apps
          (with-temp-buffer
            (unless (eql 0 (call-process "osascript" nil t nil "-e" script))
              (error "Failed to list running applications: %s"
                     (string-trim (buffer-string))))
            (split-string (string-trim (buffer-string)) ", " t))))
    (mapcar
     (lambda (app)
       (list :id app
             :title app
             :icon (launcher-icons-app app)
             :data app))
     (delete-dups apps))))

(defun my/quit-app-submit (candidate)
  "Ask the selected application to quit."
  (let ((app (plist-get candidate :data)))
    (make-process
     :name "quit-app"
     :buffer nil
     :command
     (list "osascript" "-e"
           (format "tell application %S to quit" app))
     :noquery t
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (unless (and (eq (process-status proc) 'exit)
                      (zerop (process-exit-status proc)))
           (message "Failed to request quit: %s" app)))))
    nil))

(with-eval-after-load 'launcher
  (add-to-list
   'launcher-tools
   `("quit" :name "Quit App"
     :icon ,(when (fboundp 'nerd-icons-faicon)
              (nerd-icons-faicon "nf-fa-power_off"))
     :candidates my/quit-app-candidates
     :submit my/quit-app-submit)))

(provide 'init-launcher)
;;; init-launcher.el ends here
