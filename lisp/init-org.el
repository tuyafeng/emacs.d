;;; init-org.el --- Org-mode configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org
  :ensure nil
  :defer t
  :config
  (setq org-link-descriptive 'nil)
  (setq org-display-custom-times t)
  (setq org-time-stamp-custom-formats
        '("<%Y-%m-%d %H:%M>" . "<%Y-%m-%d %H:%M:%S>"))
  (setq org-log-into-drawer t)
  (setq org-export-with-drawers '("LOGBOOK"))
  (setq org-image-actual-width 'nil)
  (setq org-cycle-separator-lines -1)
  (setq org-list-allow-alphabetical t)
  (setq org-export-with-section-numbers nil)
  (setq org-modules nil)
  (setq org-export-backends '(ascii html))

  ;; Reference: https://emacs-china.org/t/org-babel/18699/10
  (defun my/org-babel-execute-src-block (&optional _arg info _params)
    "Lazy-load Org Babel language support before executing a code block."
    (let* ((lang (nth 0 info))
           (sym (if (member (downcase lang) '("c" "cpp" "c++")) 'C (intern lang)))
           (backup-languages org-babel-load-languages))
      (unless (assoc sym backup-languages)
        (condition-case err
            (progn
              (org-babel-do-load-languages 'org-babel-load-languages (list (cons sym t)))
              (setq-default org-babel-load-languages (append (list (cons sym t)) backup-languages)))
          (file-missing
           (setq-default org-babel-load-languages backup-languages)
           err)))))
  (advice-add 'org-babel-execute-src-block :before #'my/org-babel-execute-src-block)

  (defun my/org-align-all-tables ()
    "Align all tables in the current org buffer."
    (interactive)
    (org-table-map-tables 'org-table-align 'quiet))

  :custom-face
  (org-level-1 ((t (:height 1.15))))
  (org-level-2 ((t (:height 1.13))))
  (org-level-3 ((t (:height 1.11))))
  (org-level-4 ((t (:height 1.09))))
  (org-level-5 ((t (:height 1.07))))
  (org-level-6 ((t (:height 1.05))))
  (org-level-7 ((t (:height 1.03))))
  (org-level-8 ((t (:height 1.01)))))

(use-package ox-html
  :ensure nil
  :defer t
  :config
  (setq org-html-validation-link nil)
  (setq org-html-postamble t)
  (setq org-html-postamble-format
        '(("en" "<p class=\"author\">Author: %a</p>
<p class=\"date\">Created: %d</p>
<p class=\"date\">Last Updated: %C</p>"))))

(use-package org-download
  :commands (org-download-clipboard
             org-download-yank
             org-download-screenshot
             org-download-image)
  :hook (org-mode . my/org-download-org-mode-hook)
  :config
  (setq org-download-heading-lvl nil)
  (setq org-download-image-attr-list
        '("#+caption: "
          "#+attr_org: :width 300px"
          "#+attr_html: :width 50% :align center"))
  (defun dummy-org-download-annotate-function (link) "")
  (setq org-download-annotate-function
        #'dummy-org-download-annotate-function)
  (defun my/org-download-org-mode-hook()
    (when buffer-file-name
      (setq-local org-download-heading-lvl nil)
      (setq-local org-download-image-dir
                  (concat "./" (file-name-base buffer-file-name) ".assets")))))

(use-package calendar
  :ensure nil
  :hook (calendar-today-visible . calendar-mark-today)
  :config
  (setq calendar-chinese-all-holidays-flag t)
  (setq calendar-week-start-day 1))

(use-package ox-hugo
  :defer t
  :after ox)

(defun my/paste-markdown-as-org ()
  "Paste clipboard Markdown content as Org-mode, converted via pandoc."
  (interactive)
  (let ((org-output
         (shell-command-to-string
          (format "pandoc -f markdown -t org <<'EOF'\n%s\nEOF"
                  (or (current-kill 0) "")))))
    (if (string-blank-p org-output)
        (user-error "Clipboard empty or pandoc conversion failed")
      (when (use-region-p)
        (delete-region (region-beginning) (region-end)))
      (insert org-output))))

(global-set-key (kbd "C-c p o") #'my/paste-markdown-as-org)

(provide 'init-org)
;;; init-org.el ends here
