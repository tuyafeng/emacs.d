;;; init-launcher.el --- Launcher -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package launcher
  :defer t
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

  (setq launcher-app-directories
        '("/Applications"
          "/System/Applications"
          "/System/Library/CoreServices"
          "~/Applications"))

  (setq launcher-bangs
        '(("g" "Google" "https://www.google.com.hk/search?newwindow=1&q=%s")
          ("bd" "Baidu" "https://www.baidu.com/s?ie=UTF-8&wd=%s")
          ("ddg" "DuckDuckGo" "https://duckduckgo.com/?ia=web&q=%s")
          ("bl" "Baidu" "https://search.bilibili.com/all?keyword=%s")
          ("yt" "YouTube" "https://www.youtube.com/results?search_query=%s")
          ("gh" "GitHub" "https://github.com/search?type=repositories&s=stars&o=desc&q=%s")
          ("ec" "Emacs China" "https://www.google.com.hk/search?newwindow=1&q=%s+site:emacs-china.org")
          ("vx" "V2EX" "https://www.google.com.hk/search?newwindow=1&q=%s+site:v2ex.com")))

  (setq launcher-fallback-search-url "https://www.google.com.hk/search?newwindow=1&q=%s")
  (setq browse-url-browser-function #'browse-url-default-browser))

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

;; My system launcher popup frame

(defvar my/launcher-next-command nil
  "Command to run after Launcher closes.")

(defun my/launcher-setup ()
  "Configure Launcher minibuffer shortcuts."
  (dolist (binding '(("C-x d" . dired)
                     ("C-x C-f" . find-file)
                     ("C-x b" . consult-buffer)
                     ("C-c c" . my/calc-live)
                     ("M-x" . execute-extended-command)
                     ("<escape>" . nil)))
    (let ((command (cdr binding)))
      (keymap-local-set
       (car binding)
       (lambda ()
         (interactive)
         (setq my/launcher-next-command command)
         (abort-recursive-edit))))))

(defun my/launcher-popup ()
  "Open Launcher in a temporary minibuffer frame."
  (interactive)
  (let* ((original (selected-frame))
         (frame (make-frame
                 '((name . "Launcher")
                   (width . 72)
                   (height . 10)
                   (minibuffer . only)
                   (undecorated . t)
                   (internal-border-width . 12)
                   (menu-bar-lines . 0)
                   (tool-bar-lines . 0)
                   (tab-bar-lines . 0)
                   (vertical-scroll-bars . nil)
                   (left-fringe . 0)
                   (right-fringe . 0)
                   (cursor-type . bar)))))
    (select-frame-set-input-focus frame)
    (run-at-time 0.05 nil #'my/launcher-popup-run frame original)))

(defun my/launcher-popup-run (frame original)
  "Run Launcher in FRAME, then execute commands in ORIGINAL."
  (when (frame-live-p frame)
    (let (my/launcher-next-command)
      (unwind-protect
          (condition-case nil
              (with-selected-frame frame
                (select-frame-set-input-focus frame)
                (let ((default-minibuffer-frame frame))
                  (minibuffer-with-setup-hook #'my/launcher-setup
                    (call-interactively #'launcher))))
            (quit nil))
        (when (frame-live-p frame)
          (delete-frame frame)))
      (when (and my/launcher-next-command
                 (frame-live-p original))
        (select-frame-set-input-focus original)
        (with-selected-frame original
          (call-interactively my/launcher-next-command))))))

(provide 'init-launcher)
;;; init-launcher.el ends here
