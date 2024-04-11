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

(when (eq system-type 'darwin)
  (defun my/reveal-current-file-in-finder ()
    "Reveal current file in Finder."
    (interactive)
    (let ((file (if (derived-mode-p 'dired-mode)
                    (or (dired-get-filename nil 'noerror) (dired-current-directory))
                  (buffer-file-name))))
      (if file
          (shell-command (format "open -R %s" (shell-quote-argument file)))
        (shell-command "open .")))))

(provide 'init-utils)
;;; init-utils.el ends here
