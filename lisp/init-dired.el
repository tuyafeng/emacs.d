;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dired
  :ensure nil
  :commands (dired)
  :config
  ;; Reference: https://github.com/doomemacs/doomemacs/blob/master/modules/emacs/dired/config.el
  (let ((args (list "-ahl" "--group-directories-first")))
    (when (eq system-type 'darwin)
      ;; Use GNU ls as `gls' from `coreutils' if available.
      (if-let (gls (executable-find "gls"))
          (setq insert-directory-program gls)
        ;; BSD ls doesn't support -v or --group-directories-first
        (setq args (list (car args))
              dired-use-ls-dired nil)))
    (setq dired-listing-switches (string-join args " ")))

  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-dwim-target t)
  (set-face-bold 'dired-directory t)
  (setq delete-by-moving-to-trash t)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dired-movement-style 'cycle)

  ;; Remeber `dired-hide-details-mode`
  (setq my/dired-hide-details-mode-value 1)

  (defun my/dired-hide-details-mode-hook ()
    (when (eq major-mode 'dired-mode)
      (dired-hide-details-mode my/dired-hide-details-mode-value)))

  (add-hook 'dired-after-readin-hook #'my/dired-hide-details-mode-hook)

  (defun my/dired-toggle-dired-hide-details-mode ()
    "Toggle `dired-hide-details-mode` and remember its state."
    (interactive)
    (setq my/dired-hide-details-mode-value
          (if (= my/dired-hide-details-mode-value 1) -1 1))
    (my/dired-hide-details-mode-hook))

  (define-key dired-mode-map (kbd "(")
              #'my/dired-toggle-dired-hide-details-mode))

(use-package dired-x
  :ensure nil
  :after dired
  :hook (dired-mode . dired-omit-mode)
  :bind (:map dired-mode-map
              ("s->" . dired-omit-mode))
  :config
  (setq dired-omit-files "^\\\..*")
  (setq dired-omit-verbose nil)
  (defun my/dired-omit-startup-after-advice()
    (diminish 'dired-omit-mode ""))
  (advice-add 'dired-omit-startup :after 'my/dired-omit-startup-after-advice)
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (when-let (cmd (cond ((eq system-type 'darwin) "open")
                       ((eq system-type 'gnu/linux) "xdg-open")
                       ((eq system-type 'windows-nt) "start")))
    (setq dired-guess-shell-alist-user
          `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd)))))

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle))
  :config
  ;; Revert buffer after subtree toggle
  (defun my/dired-subtree-toggle-after-advice()
    (revert-buffer))
  (advice-add 'dired-subtree-toggle :after #'my/dired-subtree-toggle-after-advice))

(use-package fd-dired
  :defer t)

;; In Emacs 29 the NS port will have a system trash implementation:
;; https://github.com/emacs-mirror/emacs/commit/796075ef7e1c7a294fe8c3c36c999c10c2f09d38
(when (and (< emacs-major-version 29) (eq system-type 'darwin))
  (use-package osx-trash
    :after dired
    :config
    (osx-trash-setup)))

(use-package doc-view
  :ensure nil
  :defer t
  :config
  (setq doc-view-resolution 300))

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-subtree-line-prefix "  ")
  (setq dired-sidebar-theme 'nerd)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-width 30)
  (defun my/dired-sidebar-mode-hook ()
    ;; Don't wrap lines
    (visual-line-mode -1)
    ;; Refresh buffer when visiting local directory.
    (unless (file-remote-p default-directory)
      (auto-revert-mode)))
  (add-hook 'dired-sidebar-mode-hook #'my/dired-sidebar-mode-hook))

(use-package nerd-icons-dired
  :after (dired nerd-icons)
  :hook (dired-mode . nerd-icons-dired-mode)
  :diminish nerd-icons-dired-mode)

;; Use space to quicklook file on macOS
(when (eq system-type 'darwin)
  (defun my/quicklook-file ()
    "Use QuickLook to preview current file."
    (interactive)
    (let ((file (if (derived-mode-p 'dired-mode)
                    (dired-get-filename)
                  (buffer-file-name))))
      (when file
        (start-process "quicklook" nil "qlmanage" "-p" file))))
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "SPC") #'my/quicklook-file)))

(defun my/copy-file-to-clipboard (&optional file)
  "Copy the file at point to the clipboard.
If FILE is provided, copy it. Otherwise, use the file at point in `dired-mode` or the current buffer's file."
  (interactive)
  (let ((file (or file (if (derived-mode-p 'dired-mode)
                           (dired-get-file-for-visit)
                         (buffer-file-name)))))
    (if (not (and file (file-regular-p file)))
        (message "No valid file found.")
      (cond
       ;; Windows
       ((eq system-type 'windows-nt)
        (if (zerop (call-process-shell-command
                    (format "powershell -Command \"Set-Clipboard -Path '%s'\""
                            (replace-regexp-in-string "/" "\\" (expand-file-name file) t t))))
            (message "Copied %s to clipboard" file)
          (message "Failed to copy %s to clipboard" file)))

       ;; macOS
       ((eq system-type 'darwin)
        (let ((script (format "set the clipboard to POSIX file \"%s\""
                              (expand-file-name file))))
          (do-applescript script)
          (message "Copied %s to clipboard" file)))

       ;; Linux
       ((eq system-type 'gnu/linux)
        (let ((mime-type (mailcap-extension-to-mime (file-name-extension file))))
          (if (and mime-type
                   (zerop (call-process-shell-command
                           (format "xclip -selection clipboard -t %s -i %s"
                                   mime-type (shell-quote-argument file)))))
              (message "Copied %s to clipboard" file)
            (message "Failed to copy %s to clipboard" file))))

       ;; Unsupported system
       (t
        (message "Clipboard copy is not supported on this system."))))))

(defun my/reveal-current-file-externally ()
  "Reveal current file in system file manager."
  (interactive)
  (when-let ((file (if (derived-mode-p 'dired-mode)
                       (or (dired-get-filename nil 'noerror)
                           (dired-current-directory))
                     (buffer-file-name))))
    (cond
     ((eq system-type 'windows-nt)
      (shell-command
       (format "explorer /select, %s"
               (string-replace "/" "\\" (shell-quote-argument file)))))
     ((eq system-type 'darwin)
      (shell-command
       (format "open -R %s" (shell-quote-argument file))))
     (t
      (if (featurep 'embark)
          (embark-open-externally (file-name-directory file))
        (message (format "Cannot reveal file at %s" file)))))))

(provide 'init-dired)
;;; init-dired.el ends here
