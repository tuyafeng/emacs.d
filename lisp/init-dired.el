;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(use-package dired
  :ensure nil
  :commands (dired)
  :config

  ;; Show directories first, reference: https://www.emacswiki.org/emacs/DiredSortDirectoriesFirst
  (defun my/dired-sort ()
    "Sort dired listings with directories first."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2) ;; beyond dir. header
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
      (set-buffer-modified-p nil)))
  (defadvice dired-readin
      (after dired-after-updating-hook first () activate)
    "Sort dired listings with directories first before adding marks."
    (my/dired-sort))

  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-alhF")
  (set-face-bold 'dired-directory t)
  (when (eq system-type 'darwin)
    (setq dired-use-ls-dired nil))
  (setq delete-by-moving-to-trash t)
  ;; Open directory and file in current buffer
  ;;(put 'dired-find-alternate-file 'disabled nil)
  ;;:bind (:map dired-mode-map
  ;;            ([remap dired-find-file] . dired-find-alternate-file))
  )

(use-package dired-x
  :ensure nil
  :after dired
  :hook
  (dired-mode . (lambda () (dired-omit-mode 1)))
  :config
  (setq dired-omit-files "^\\\..*")
  (setq dired-omit-verbose nil)

  (when (eq system-type 'darwin)
    ;; OS X bsdtar is mostly compatible with GNU Tar
    (setq dired-guess-shell-gnutar "tar"))

  (setq dired-guess-shell-alist-user `((,(rx "."
                                             (or
                                              ;; Videos
                                              "mp4" "avi" "mkv" "flv" "ogv" "mov"
                                              ;; Music
                                              "wav" "mp3" "flac"
                                              ;; Images
                                              "jpg" "jpeg" "png" "gif" "xpm" "svg" "bmp"
                                              ;; Docs
                                              "pdf" "md" "djvu" "ps" "eps")
                                             string-end)
                                        ,(cond ((eq system-type 'gnu/linux) "xdg-open")
                                               ((eq system-type 'darwin) "open")
                                               ((eq system-type 'windows-nt) "start")
                                               (t "")))))

  ;; Diminish dired-omit-mode. We need this hack, because Dired Omit Mode has
  ;; a very peculiar way of registering its lighter explicitly in
  ;; `dired-omit-startup'.  We can't just use `:diminish' because the lighter
  ;; isn't there yet after dired-omit-mode is loaded.
  (add-function :after (symbol-function 'dired-omit-startup)
                (lambda () (diminish 'dired-omit-mode ""))
                '((name . dired-omit-mode-diminish))))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)))

(use-package fd-dired
  :ensure t
  :defer t)

;; Reference: https://github.com/gmoutso/dotemacs/blob/93649716da46497dd79d07e06a30e694b9207a2b/lisp/variousrc.el
(defun my/copy-file-to-clipboard (&optional file)
  "Copy file at point to clipboard.
  This function recognizes dired-mode files and image-mode buffers."
  (interactive)
  (let ((file (or file
                  (cond
                   ((derived-mode-p 'dired-mode) (dired-copy-filename-as-kill))
                   ((derived-mode-p 'image-mode) (buffer-file-name))))))
    (when file
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
                 file)))))
    (message "Copied %s" file)))

(use-package osx-trash
  :if (eq system-type 'darwin)
  :ensure t
  :init (osx-trash-setup))

(setq doc-view-resolution 300)

(provide 'init-dired)
;;; init-dired.el ends here
